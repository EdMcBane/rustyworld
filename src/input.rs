use std::collections::HashSet;

#[derive(PartialEq, Eq, Hash)]
pub enum Input {
    UP,
    DOWN,
    LEFT,
    RIGHT,
    BUTTON,
}

pub trait InputDevice {
    fn read_input(&self) -> HashSet<Input>;
}