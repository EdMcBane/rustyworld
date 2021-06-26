pub trait Audio {
    fn play_sound(&mut self, data: &[u8], freq: u8, vol: u8, channel: u8);
    fn stop(&mut self);
}

