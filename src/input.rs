use std::collections::HashSet;
use minifb::{Key, Window};
use std::rc::Rc;
use std::cell::RefCell;
use crate::input_defs::Input;

pub trait InputDevice {
    fn read_input(&self) -> HashSet<Input>;
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
