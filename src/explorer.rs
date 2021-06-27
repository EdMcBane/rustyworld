pub use crate::ResType;
use std::rc::Rc;
use crate::resources::ResourceManager;
use crate::{FileResourceManager, DefaultMusic};
use crate::rodio::{RodioAudio, RodioMusicAdapter};
use crate::audio::Audio;
use std::error::Error;
use crate::music::Music;
use std::time::Duration;

pub struct Explorer {
    resman: Rc<FileResourceManager>,
    audio: RodioAudio,
    music: DefaultMusic<FileResourceManager, RodioMusicAdapter>,
}

impl Explorer {
    pub fn new() -> Result<Self, Box<dyn Error>> {
        let resman = Rc::new(FileResourceManager::new()?);
        // Preload all resources, since there is no bytecode to do it for us
        resman.resources.iter().enumerate()
            .for_each(|(i, _)| { resman.preload_resource(i as u8); });
        let mut explorer = Self {
            resman: resman.clone(),
            audio: RodioAudio::new(),
            music: DefaultMusic::new(resman, RodioMusicAdapter::new(1)),
        };
        Ok(explorer)
    }
    pub fn list_resources_by_type(&mut self, res_type: ResType) -> Vec<u8> {
        self.resman.resources.iter()
            .enumerate()
            .filter(|(i, r)| r.etype == res_type)
            .map(|(i,_)| i as u8)
            .collect()
    }

    pub fn play_sound(&mut self, resource_id: u8, freq: u8) -> Result<(), Box<dyn Error>>{
        let data= self.resman.read_resource(resource_id)?;
        self.audio.play_sound(&data, freq, 0x40, 0);
        Ok(())
    }

    pub fn play_song(&mut self, resource_id: u8, beat_interval: Option<Duration>) -> Result<(), Box<dyn Error>> {
        self.music.start(resource_id, beat_interval.map(|b| (b.as_millis() as u64 * 7050 / 60) as u16), 0);
        Ok(())
    }
}
