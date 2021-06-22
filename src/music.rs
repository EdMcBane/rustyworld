use rodio::{Sink, OutputStream, Source, OutputStreamHandle, Sample};
use byteorder::BigEndian;
use byteorder::ReadBytesExt;
use std::time::Duration;
use crate::resources::{ResourceManager};
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::sync::mpsc::{Receiver, channel, Sender};
use crate::vm::SPEED_UP;

pub trait Music {
    fn start(&mut self, res_id: u16, delay: Option<u16>, pos: u8);
    fn delay(&mut self, delay: u16);
    fn stop(&mut self);
    fn channel(&mut self) -> &mut Receiver<i16>;
}

#[derive(Debug)]
struct Instrument {
    volume: u16,
    data: Vec<u8>,
    len: usize,
    loop_len: usize,

}

struct SfxModule {
    orders: Vec<u8>,
    instruments: Vec<Instrument>,
    beats: Vec<Beat>,
}

struct SfxChan {
    channel: usize,
    beat_interval_ms: Arc<AtomicUsize>,
    remaining: usize,
    module: Arc<SfxModule>,
    position: usize,
    order_index: usize,
    playback_state: Option<PlaybackState>,
    sender: Sender<i16>,
}

struct PlaybackState {
    instrument_id: usize,
    sample_rate: u32,
    vol: f32,
    sample_index: usize,
}

#[derive(Copy, Clone)]
enum Beat {
    Sync(u16),
    Stop,
    Note {
        sample_rate: u32,
        instrument_id: usize,
        volume_adjust: i16,
    },
    Noop,
}


const DEFAULT_SAMPLE_RATE: u32 = 8000;

impl SfxChan {
    fn new(channel: usize, beat_interval_ms: Arc<AtomicUsize>, sender: Sender<i16>, module: Arc<SfxModule>, order_index: usize) -> Self {
        let samples_per_beat = DEFAULT_SAMPLE_RATE * beat_interval_ms.load(Ordering::SeqCst) as u32 / 1000;

        SfxChan {
            channel,
            beat_interval_ms,
            remaining: samples_per_beat as usize,
            module,
            position: 0,
            order_index,
            playback_state: None,
            sender,
        }
    }

    fn next_timeslot(&mut self) {
        if self.order_index < self.module.orders.len() {
            let order = self.module.orders[self.order_index] as usize;
            let offset = order * 256 + self.position + self.channel;
            self.process_note(self.module.beats[offset]);
            self.position += 4;
            if self.position >= 256 {
                self.position = 0;
                self.order_index += 1;
                if self.order_index == self.module.orders.len() {
                    self.playback_state.take();
                }
            }
        }
        let samples_per_beat = self.sample_rate() * self.beat_interval_ms.load(Ordering::SeqCst) as u32 / 1000;
        self.remaining = samples_per_beat as usize / SPEED_UP;
    }

    fn process_note(&mut self, beat: Beat) {
        match beat {
            Beat::Sync(val) => {
                eprintln!("note: Sync");
                self.sender.send(val as i16).unwrap();
            }
            Beat::Stop => {
                eprintln!("note: Stop");
                self.playback_state.take();
            }
            Beat::Note { sample_rate, instrument_id, volume_adjust } => {
                eprintln!("processing note for channel {} instrument {:?}, vol_adj {} sample_rate {}", self.channel, instrument_id, volume_adjust, sample_rate);
                let instrument = &self.module.instruments[instrument_id];
                let vol = num::clamp(instrument.volume as i16 + volume_adjust, 0, 0x40) as u16;
                self.playback_state = Some(PlaybackState {
                    instrument_id,
                    sample_rate,
                    vol: vol as f32 / 64.,
                    sample_index: 0,
                })
            }
            Beat::Noop => {
                // eprintln!("note: noop on channel {}", self.channel);
            }
        }
    }
}

impl Iterator for SfxChan {
    type Item = i16;

    fn next(&mut self) -> Option<Self::Item> {
        let result = if let Some(playback_state) = &mut self.playback_state {
            let instrument = &self.module.instruments[playback_state.instrument_id];
            let sample = instrument.data[playback_state.sample_index];
            let vol = playback_state.vol;
            playback_state.sample_index += 1;
            if playback_state.sample_index == instrument.data.len() {
                if instrument.loop_len != 0 {
                    playback_state.sample_index = instrument.len;
                } else {
                    self.playback_state.take();
                }
            }
            Some(((sample as i16) << 8).amplify(vol))
        } else {
            Some(0)
        };
        self.remaining -= 1;
        if self.remaining == 0 {
            self.next_timeslot()
        }
        result
    }
}

impl Source for SfxChan {
    fn current_frame_len(&self) -> Option<usize> {
        Some(self.remaining)
    }

    fn channels(&self) -> u16 {
        1
    }

    fn sample_rate(&self) -> u32 {
        self.playback_state.as_ref().map(|e| e.sample_rate).unwrap_or(DEFAULT_SAMPLE_RATE)
    }

    fn total_duration(&self) -> Option<Duration> {
        None
    }
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

pub struct RodioMusic<R> {
    pub os: OutputStream,
    pub stream_handle: OutputStreamHandle,
    pub channels: [Sink; 4],
    pub beat_interval_ms: Arc<AtomicUsize>,
    pub resman: Rc<R>,
    pub sender: Sender<i16>,
    pub receiver: Receiver<i16>,
}

impl<R: ResourceManager> RodioMusic<R> {
    pub fn new(resman: Rc<R>) -> RodioMusic<R> {
        let (os, stream_handle) = OutputStream::try_default().unwrap();
        let channels = [
            Sink::try_new(&stream_handle).unwrap(),
            Sink::try_new(&stream_handle).unwrap(),
            Sink::try_new(&stream_handle).unwrap(),
            Sink::try_new(&stream_handle).unwrap(),
        ];
        let (tx, rx) = channel::<i16>();
        RodioMusic {
            os,
            stream_handle,
            channels,
            beat_interval_ms: Arc::new(AtomicUsize::new(0)),
            resman,
            sender: tx,
            receiver: rx,
        }
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

    fn parse_beats(&self, mut data: &[u8]) -> Vec<Beat> {
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

impl<R: ResourceManager> Music for RodioMusic<R> {
    fn start(&mut self, resource_id: u16, requested_delay: Option<u16>, order_index: u8) {
        eprintln!("snd_playMusic {} {:?} {}", resource_id, requested_delay, order_index);
        if let Some(data) = self.resman.resource(resource_id as u8) {

            // 0x00 - 0x02 : u16 delay
            // 0x02 - 0x3E : [(u16, u16); 15] instrument res & vol
            // 0x3E - 0x3F : order_count
            // 0x40 - 0xC0 : [u8; 0x80] order_table
            let default_delay = (&data[0x3E..]).read_u16::<BigEndian>().unwrap();
            let delay = requested_delay.unwrap_or(default_delay) as u64;
            let delay = Duration::from_millis(delay * 60 / 7050);
            let module = Arc::new(SfxModule {
                orders: (&data[0x40..0xC0]).iter().take((&data[0x3E..]).read_u16::<BigEndian>().unwrap() as usize).cloned().collect(),
                instruments: self.parse_instruments(&data[0x02..]),
                beats: self.parse_beats(&data[0xC0..]),
            });
            self.beat_interval_ms = Arc::new(AtomicUsize::new(delay.as_millis() as usize));
            for c in 0..4 {
                let channel = &mut self.channels[c as usize];
                *channel = Sink::try_new(&self.stream_handle).unwrap();
                let chan_src = SfxChan::new(c, self.beat_interval_ms.clone(), self.sender.clone(), module.clone(), order_index as usize);
                channel.append(chan_src);
            }
        }
    }


    fn delay(&mut self, delay: u16) {
        let delay = Duration::from_millis(delay as u64 * 60 / 7050);
        self.beat_interval_ms.store(delay.as_millis() as usize, Ordering::SeqCst);
    }

    fn stop(&mut self) {
        for chan in self.channels.iter_mut() {
            *chan = Sink::try_new(&self.stream_handle).unwrap();
        }
    }

    fn channel(&mut self) -> &mut Receiver<i16> {
        &mut self.receiver
    }
}

#[cfg(test)]
mod tests {
    use crate::music::*;
    use std::sync::mpsc::channel;
    use std::sync::Arc;

    #[test]
    fn returns_zero_until_note() {
        let (tx, _rx) = channel();
        let mut sut = SfxChan::new(0, Arc::new(AtomicUsize::new(10)), tx,Arc::new(SfxModule {
            instruments: vec![Instrument {
                volume: 0x40,
                data: (0..1024).into_iter().map(|e| (e % 256) as u8).collect(),
                len: 1023,
                loop_len: 1,
            }],
            orders: vec![0, 1, 2, 3],
            beats: vec![
                Beat::Note {
                    sample_rate: 1024,
                    instrument_id: 0,
                    volume_adjust: 0,
                },
                Beat::Noop,
                Beat::Noop,
                Beat::Noop,
                Beat::Note {
                    sample_rate: 2048,
                    instrument_id: 0,
                    volume_adjust: 1,
                },
            ],
        }), 0);

        assert_eq!(8000, sut.sample_rate());
        assert_eq!(80, sut.remaining);
        for _ in 0..80 { // 8khz, 10ms = 80 samples
            assert_eq!(Some(0), sut.next());
        }
        for i in 0..10 { // 1024hz, 10ms = 10 samples
            assert_eq!(Some((i % 256) << 8), sut.next());
        }
        assert_eq!(Some(0), sut.next());
    }

    #[test]
    fn loop_works() {
        let (tx, _rx) = channel();
        let mut sut = SfxChan::new(0, Arc::new(AtomicUsize::new(1000)), tx, Arc::new(SfxModule {
            instruments: vec![Instrument {
                volume: 0x40,
                data: (0..1000).into_iter().map(|e| (e % 256) as u8).collect(),
                len: 999,
                loop_len: 1,
            }],
            orders: vec![0, 1, 2, 3],
            beats: vec![
                Beat::Note {
                    sample_rate: 8000,
                    instrument_id: 0,
                    volume_adjust: 0,
                },
            ],
        }), 0);

        for _ in 0..8000 { // 8khz, 1000ms = 8000 samples
            assert_eq!(Some(0), sut.next());
        }
        for i in 0..1000 {
            assert_eq!(Some((i % 256) << 8), sut.next());
        }
        for _ in 0..1000 {
            assert_eq!(Some((999 % 256) << 8), sut.next());
        }
    }
}