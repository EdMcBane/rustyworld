mod resources;
mod vm;
mod video;
mod audio;
mod strings;
mod music;
mod input;

#[cfg(feature = "minifb")]
pub mod minifb;
#[cfg(feature = "rodio")]
pub mod rodio;
#[cfg(all(feature = "rodio", feature = "minifb"))]
pub mod explorer;

pub use crate::vm::Vm;
pub use crate::resources::{FileResourceManager, ResType};
pub use crate::video::{DefaultVideo, WIDTH, HEIGHT};
pub use crate::music::DefaultMusic;