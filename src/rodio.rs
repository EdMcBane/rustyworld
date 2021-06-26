use rodio::{Sink, OutputStream, Source, OutputStreamHandle, Sample};
use byteorder::BigEndian;
use byteorder::ReadBytesExt;
use rodio::buffer::SamplesBuffer;
use crate::resources::{ResourceManager};
use std::rc::Rc;
use crate::audio::Audio;
use std::time::Duration;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::sync::mpsc;
use crate::music::{Song, Beat, MusicAdapter, BeatIter};

pub struct RodioAudio<R> {
    pub os: OutputStream,
    // Necessary to keep alive
    pub stream_handle: OutputStreamHandle,
    pub channels: [Sink; 4],
    pub resman: Rc<R>,
}

static FREQ_TABLE: [u16; 40] = [
    0x0CFF, 0x0DC3, 0x0E91, 0x0F6F, 0x1056, 0x114E, 0x1259, 0x136C,
    0x149F, 0x15D9, 0x1726, 0x1888, 0x19FD, 0x1B86, 0x1D21, 0x1EDE,
    0x20AB, 0x229C, 0x24B3, 0x26D7, 0x293F, 0x2BB2, 0x2E4C, 0x3110,
    0x33FB, 0x370D, 0x3A43, 0x3DDF, 0x4157, 0x4538, 0x4998, 0x4DAE,
    0x5240, 0x5764, 0x5C9A, 0x61C8, 0x6793, 0x6E19, 0x7485, 0x7BBD
];

impl<R: ResourceManager> RodioAudio<R> {
    pub fn new(resman: Rc<R>) -> RodioAudio<R> {
        let (os, stream_handle) = OutputStream::try_default().unwrap();
        let channels = [
            Sink::try_new(&stream_handle).unwrap(),
            Sink::try_new(&stream_handle).unwrap(),
            Sink::try_new(&stream_handle).unwrap(),
            Sink::try_new(&stream_handle).unwrap(),
        ];
        RodioAudio {
            os,
            stream_handle,
            channels,
            resman,
        }
    }
}

impl<R: ResourceManager> Audio for RodioAudio<R> {
    fn play_sound(&mut self, data: &[u8], freq: u8, mut vol: u8, channel_id: u8) {
        eprintln!("Playing chan {} sound, freq {} vol {}", channel_id, freq, vol);
        assert!(freq < 0x40);
        // assert!(vol < 0x40);
        vol = vol.min(0x40);
        assert!(channel_id < 4);
        let channel = &mut self.channels[channel_id as usize];
        *channel = Sink::try_new(&self.stream_handle).unwrap();
        if vol == 0 {
            return;
        }
        let mut mydata = data;
        let len = mydata.read_u16::<BigEndian>().unwrap() * 2;
        let loop_len = mydata.read_u16::<BigEndian>().unwrap() * 2;

        let content = &data[8..];
        let sample_rate = FREQ_TABLE[freq as usize];

        let once = SamplesBuffer::new(1, sample_rate as u32, convert(&content[..len as usize]));
        channel.append(once.amplify(vol as f32 / 64.));
        if loop_len != 0 {
            let forever = SamplesBuffer::new(1, sample_rate as u32, convert(&content[(len as usize)..])).repeat_infinite();
            channel.append(forever.amplify(vol as f32 / 64.));
        }
    }

    fn stop(&mut self) {
        for channel in self.channels.iter_mut() {
            *channel = Sink::try_new(&self.stream_handle).unwrap()
        }
    }
}

fn convert(data: &[u8]) -> Vec<i16> {
    data.iter().map(|&s| (s as i16) << 8).collect::<Vec<i16>>() // TODO: streaming
}


struct SfxChan {
    speed_up: usize,
    channel: usize,
    beat_interval_ms: Arc<AtomicUsize>,
    remaining: usize,
    module: Arc<Song>,
    beats: BeatIter,
    playback_state: Option<PlaybackState>,
    sender: mpsc::Sender<i16>,
}

struct PlaybackState {
    instrument_id: usize,
    sample_rate: u32,
    vol: f32,
    sample_index: usize,
}

const DEFAULT_SAMPLE_RATE: u32 = 8000;

impl SfxChan {
    fn new(channel: usize, speed_up: usize, beat_interval_ms: Arc<AtomicUsize>, sender: mpsc::Sender<i16>, module: Arc<Song>) -> Self {
        let samples_per_beat = DEFAULT_SAMPLE_RATE * beat_interval_ms.load(Ordering::SeqCst) as u32 / 1000;

        SfxChan {
            speed_up,
            channel,
            beat_interval_ms,
            remaining: samples_per_beat as usize,
            module: module.clone(),
            beats: Song::iter_chan(module, channel),
            playback_state: None,
            sender,
        }
    }

    fn next_timeslot(&mut self) {
        if let Some(beat) = self.beats.next() {
            self.process_note(beat);
        } else {
            self.playback_state.take();
        }
        let samples_per_beat = self.sample_rate() * self.beat_interval_ms.load(Ordering::SeqCst) as u32 / 1000;
        self.remaining = samples_per_beat as usize / self.speed_up;
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

pub struct RodioMusicAdapter {
    speed_up: usize,
    _os: OutputStream,
    stream_handle: OutputStreamHandle,
    channels: [Sink; 4],
    beat_interval_ms: Arc<AtomicUsize>,
    sender: mpsc::Sender<i16>,
    receiver: mpsc::Receiver<i16>,
}

impl RodioMusicAdapter {
    pub fn new(speed_up: usize) -> RodioMusicAdapter {
        let (os, stream_handle) = OutputStream::try_default().unwrap();
        let channels = [
            Sink::try_new(&stream_handle).unwrap(),
            Sink::try_new(&stream_handle).unwrap(),
            Sink::try_new(&stream_handle).unwrap(),
            Sink::try_new(&stream_handle).unwrap(),
        ];
        let (tx, rx) = mpsc::channel::<i16>();
        RodioMusicAdapter {
            speed_up,
            _os: os,
            stream_handle,
            channels,
            beat_interval_ms: Arc::new(AtomicUsize::new(0)),
            sender: tx,
            receiver: rx,
        }
    }
}

impl MusicAdapter for RodioMusicAdapter {
    fn start(&mut self, module: Song) {
        self.beat_interval_ms.store(module.beat_interval.as_millis() as usize, Ordering::SeqCst);
        let module = Arc::new(module);
        for c in 0..4 {
            let channel = &mut self.channels[c as usize];
            *channel = Sink::try_new(&self.stream_handle).unwrap();
            let chan_src = SfxChan::new(c, self.speed_up,self.beat_interval_ms.clone(), self.sender.clone(), module.clone());
            channel.append(chan_src);
        }
    }

    fn set_beat_interval(&mut self, beat_interval: Duration) {
        self.beat_interval_ms.store(beat_interval.as_millis() as usize, Ordering::SeqCst);
    }

    fn stop(&mut self) {
        for chan in self.channels.iter_mut() {
            *chan = Sink::try_new(&self.stream_handle).unwrap();
        }
    }

    fn latest_mark(&mut self) -> Option<i16> {
        self.receiver.try_iter().last()
    }
}

#[cfg(test)]
mod tests {
    use crate::music::*;
    use std::sync::mpsc::channel;
    use std::sync::Arc;
    use crate::rodio::SfxChan;
    use std::sync::atomic::AtomicUsize;
    use std::time::Duration;
    use rodio::Source;

    #[test]
    fn returns_zero_until_note() {
        let (tx, _rx) = channel();
        let mut sut = SfxChan::new(0, 1, Arc::new(AtomicUsize::new(10)), tx, Arc::new(Song {
            beat_interval: Duration::from_millis(10),
            start_offset: 0,
            instruments: vec![Instrument {
                volume: 0x40,
                data: (0..1024).into_iter().map(|e| (e % 256) as u8).collect(),
                len: 1023,
                loop_len: 1,
            }],
            verses: vec![0, 1, 2, 3],
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
        }));

        assert_eq!(8000, sut.sample_rate());
        assert_eq!(80, sut.remaining);
        for _ in 0..80 { // 8khz, 10ms = 80 samples
            assert_eq!(Some(0), sut.next());
        }
        assert_eq!(10, sut.remaining);
        for i in 0..10 { // 1024hz, 10ms = 10 samples
            assert_eq!(Some((i % 256) << 8), sut.next());
        }
        assert_eq!(Some(0), sut.next());
    }

    #[test]
    fn loop_works() {
        let (tx, _rx) = channel();
        let mut sut = SfxChan::new(0, 1, Arc::new(AtomicUsize::new(1000)), tx, Arc::new(Song {
            beat_interval: Duration::from_millis(1000),
            start_offset: 0,
            instruments: vec![Instrument {
                volume: 0x40,
                data: (0..1000).into_iter().map(|e| (e % 256) as u8).collect(),
                len: 999,
                loop_len: 1,
            }],
            verses: vec![0, 1, 2, 3],
            beats: vec![
                Beat::Note {
                    sample_rate: 8000,
                    instrument_id: 0,
                    volume_adjust: 0,
                },
            ],
        }));

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