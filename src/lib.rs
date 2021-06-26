#![feature(array_chunks)]
#![feature(slice_as_chunks)]
#![feature(destructuring_assignment)]
pub mod resources;
pub mod vm;
pub mod video;
pub mod audio;
mod strings;
pub mod music;
pub mod input;
#[cfg(feature = "minifb")]
pub mod minifb;
#[cfg(feature = "rodio")]
pub mod rodio;