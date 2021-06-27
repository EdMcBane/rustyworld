use byteorder::BigEndian;
use byteorder::ReadBytesExt;
use std::time::Duration;
use crate::resources::{ResourceManager};
use std::rc::Rc;
use std::sync::Arc;
use num::Integer;

pub trait Music {
    fn start(&mut self, res_id: u16, delay: Option<u16>, pos: u8);
    fn delay(&mut self, delay: u16);
    fn stop(&mut self);
    fn latest_mark(&mut self) -> Option<i16>;
}

pub trait MusicAdapter {
    fn start(&mut self, module: Song);
    fn set_beat_interval(&mut self, beat_interval: Duration);
    fn stop(&mut self);
    fn latest_mark(&mut self) -> Option<i16>;
}

pub struct DefaultMusic<R,A> {
    resman: Rc<R>,
    adapter: A,
}

impl<R: ResourceManager, A: MusicAdapter> Music for DefaultMusic<R, A> {
    fn start(&mut self, res_id: u16, delay: Option<u16>, pos: u8) {
        if let Some(module) = self.load_module(res_id, delay, pos) {
            self.adapter.start(module);
        }
    }

    fn delay(&mut self, delay: u16) {
        self.adapter.set_beat_interval(Duration::from_millis(delay as u64 * 60 / 7050))
    }

    fn stop(&mut self) {
        self.adapter.stop();
    }

    fn latest_mark(&mut self) -> Option<i16> {
        self.adapter.latest_mark()
    }
}

impl<R: ResourceManager, A:MusicAdapter> DefaultMusic<R, A> {
    pub fn new(resman: Rc<R>, adapter: A) -> Self {
        Self {
            resman,
            adapter,
        }
    }

    fn load_module(&mut self, resource_id: u16, requested_delay: Option<u16>, verse_index: u8) -> Option<Song> {
        let start_offset = verse_index as usize * 256;
        self.resman.resource(resource_id as u8).map(|data| {
            // 0x00 - 0x02 : u16 delay
            // 0x02 - 0x3E : [(u16, u16); 15] instrument res & vol
            // 0x3E - 0x3F : verse_count
            // 0x40 - 0xC0 : [u8; 0x80] verse_table
            let default_delay = (&data[0x3E..]).read_u16::<BigEndian>().unwrap();
            let delay = requested_delay.unwrap_or(default_delay) as u64;
            let beat_interval = Duration::from_millis(delay * 60 / 7050);
            Song {
                start_offset,
                beat_interval,
                verses: (&data[0x40..0xC0]).iter().take((&data[0x3E..]).read_u16::<BigEndian>().unwrap() as usize).cloned().collect(),
                instruments: self.parse_instruments(&data[0x02..]),
                beats: Self::parse_beats(&data[0xC0..]),
            }
        })
    }

    fn parse_instruments(&self, mut data: &[u8]) -> Vec<Instrument> {
        let mut instruments = vec![];
        for idx in 0..15 {
            let mut instrument: Instrument = Default::default();
            let res_id = data.read_u16::<BigEndian>().unwrap();
            let vol = data.read_u16::<BigEndian>().unwrap();
            if let Some(res) = self.resman.resource(res_id as u8) {
                instrument.volume = vol;
                instrument.len = ((&res[..]).read_u16::<BigEndian>().unwrap() * 2) as usize;
                instrument.loop_len = ((&res[2..]).read_u16::<BigEndian>().unwrap() * 2) as usize;
                instrument.data = res[8..].to_vec();
                (&mut instrument.data[0..4]).fill(0); // TODO: why?
                eprintln!("instrument {} {} {} {} {} {}", idx, res_id, instrument.volume, instrument.len, instrument.loop_len, instrument.data.len());
            }
            instruments.push(instrument);
        }
        instruments
    }

    fn parse_beats(mut data: &[u8]) -> Vec<Beat> {
        let mut beats = vec![];
        let count = data.len() / 4;
        for _ in 0..count {
            let note = data.read_u16::<BigEndian>().unwrap();
            let meta = data.read_u16::<BigEndian>().unwrap();
            beats.push(match note {
                0xFFFD => {
                    Beat::Sync(meta)
                }
                0xFFFE => {
                    Beat::Stop
                }
                _ => {
                    let sample = ((meta & 0xF000) >> 12) as usize;
                    if sample == 0 {
                        Beat::Noop
                    } else {
                        // eprintln!("preparing sample {}, note {}", sample, note);
                        let freq = 7159092 / (note as u32 * 2);
                        Beat::Note {
                            sample_rate: freq,
                            instrument_id: sample - 1,
                            volume_adjust: match (meta & 0x0F00) >> 8 {
                                5 => (meta & 0xFF) as i16,
                                6 => -((meta & 0xFF) as i16),
                                _ => 0
                            },
                        }
                    }
                }
            });
        }
        beats
    }
}

#[derive(Debug)]
pub struct Instrument {
    pub volume: u16,
    pub data: Vec<u8>,
    pub len: usize,
    pub loop_len: usize,

}

impl Default for Instrument {
    fn default() -> Self {
        Instrument {
            volume: 0,
            data: vec![],
            len: 0,
            loop_len: 0,
        }
    }
}

pub struct Song {
    pub start_offset: usize,
    pub beat_interval: Duration,
    pub verses: Vec<u8>,
    pub instruments: Vec<Instrument>,
    pub beats: Vec<Beat>,
}

impl Song {
    pub fn iter_chan<'a>(self: Arc<Song>, channel: usize) -> BeatIter { // TODO: tinytypes
        BeatIter {
            module: self.clone(),
            pos:  self.start_offset,
            channel,
        }
    }
}

pub struct BeatIter {
    module: Arc<Song>,
    pos: usize,
    channel: usize,
}

impl Iterator for BeatIter {

    type Item = Beat;

    fn next(&mut self) -> Option<Self::Item> {
        let (verse_index, pos) = self.pos.div_mod_floor(&256);
        let chan = self.channel;
        if let Some(verse) = self.module.verses.get(verse_index) {
            let offset = *verse as usize * 256 + pos + chan;
            self.pos += 4;
            Some(self.module.beats[offset])
        } else {
            None
        }

    }
}

#[derive(Copy, Clone)]
pub enum Beat {
    Sync(u16),
    Stop,
    Note {
        sample_rate: u32,
        instrument_id: usize,
        volume_adjust: i16,
    },
    Noop,
}