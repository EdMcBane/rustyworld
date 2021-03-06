use ::minifb::{ScaleMode, Window, WindowOptions};
use rustyworld::*;
use std::rc::Rc;
use std::cell::RefCell;
use rustyworld::rodio::{RodioAudio, RodioMusicAdapter};
use rustyworld::minifb::{MiniVideoAdapter, MiniInputDevice};

const SCALE: usize = 3;
const SPEED_UP: usize = 1;

fn main() {
    let window = Window::new(
        "Another rusty world",
        WIDTH as usize * SCALE,
        HEIGHT as usize * SCALE,
        WindowOptions {
            resize: false,
            scale_mode: ScaleMode::AspectRatioStretch,
            ..WindowOptions::default()
        },
    )
        .expect("Unable to create window");

    let window_ptr = Rc::new(RefCell::new(window));
    let input_device = MiniInputDevice::sharing_window(window_ptr.clone());
    let resman = Rc::new(FileResourceManager::new().unwrap());
    let audio = RodioAudio::new(resman.clone());
    let music_adapter = RodioMusicAdapter::new(SPEED_UP);
    let music = DefaultMusic::new(resman.clone(), music_adapter);
    let video_adapter = MiniVideoAdapter::sharing_window(window_ptr);
    let video = DefaultVideo::new(video_adapter);
    let mut vm = Vm::new(SPEED_UP, video, resman.clone(), input_device, Box::new(audio), Box::new(music));
    vm.run();
}

