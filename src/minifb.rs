use std::rc::Rc;
use std::cell::RefCell;
use minifb::{Key, Window};
use crate::video::{VideoAdapter, TOT_PIXELS, WIDTH, HEIGHT};

use std::collections::HashSet;
use crate::input::{Input, InputDevice};


pub struct MiniVideoAdapter {
    window: Rc<RefCell<Window>>,
    palette: Vec<u32>,
    buffer: Vec<u32>,
}

impl MiniVideoAdapter {
    pub fn new(window: Window) -> Self {
        Self::sharing_window(Rc::new(RefCell::new(window)))
    }

    pub fn sharing_window(window: Rc<RefCell<Window>>) -> Self {
        Self {
            window,
            palette: vec![0u32; 16],
            buffer: vec![0u32; TOT_PIXELS],
        }
    }

    fn palette_map(src: &[u8], dest :&mut [u32], palette: &[u32]) {
        src.iter().zip(dest.iter_mut()).for_each(|(s, d)| *d = palette[*s as usize]);
    }
}
impl VideoAdapter for MiniVideoAdapter {

    fn update_display(&mut self, image: &[u8]) {
        MiniVideoAdapter::palette_map(image, &mut self.buffer, &self.palette);
        (*self.window)
            .borrow_mut()
            .update_with_buffer(&self.buffer, WIDTH as usize, HEIGHT as usize)
            .unwrap();
    }


    fn set_palette(&mut self, palette: &[u32]) {
        self.palette = palette.to_vec();
    }
}



pub struct MiniInputDevice {
    window: Rc<RefCell<Window>>
}
impl MiniInputDevice {
    pub fn new(window: Window) -> Self{
        MiniInputDevice {
            window: Rc::new(RefCell::new(window)),
        }
    }
    pub fn sharing_window(window: Rc<RefCell<Window>>) -> Self{
        MiniInputDevice {
            window
        }
    }
}

impl InputDevice for MiniInputDevice {
    fn read_input(&self) -> HashSet<Input> {
        vec![(Input::RIGHT, Key::D),
             (Input::LEFT, Key::A),
             (Input::DOWN, Key::S),
             (Input::UP, Key::W),
             (Input::BUTTON, Key::Space)]
            .into_iter()
            .filter(|(_, k)| (*self.window).borrow().is_key_down(*k))
            .map(|p| p.0)
            .collect()
    }
}
