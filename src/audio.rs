use rodio::{Sink, OutputStream, Source, OutputStreamHandle};
use byteorder::BigEndian;
use byteorder::ReadBytesExt;
use rodio::buffer::SamplesBuffer;
use crate::resources::{ResourceManager};
use std::rc::Rc;

pub trait Audio {
    fn play_sound(&mut self, data: &[u8], freq: u8, vol: u8, channel: u8);
    fn stop(&mut self);
}


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