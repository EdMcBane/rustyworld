use rustyworld::explorer::Explorer;
use std::error::Error;
use std::time::Duration;
use std::thread::sleep;
use rustyworld::ResType;
use std::net::Shutdown::Read;
use byteorder::ReadBytesExt;

fn main() -> Result<(), Box<dyn Error>>{
    let mut editor = rustyline::Editor::<()>::new();
    let mut explorer = Explorer::new()?;
    loop {
        println!("Options:");
        println!("1. Sound");
        println!("2. Song");
        match editor.readline(">> ")?.parse::<u8>() {
            Ok(1) => {
                loop {
                    let compatible_resources = explorer.list_resources_by_type(ResType::Sound);
                    for res_id in compatible_resources.iter() {
                        println!("{}", res_id);
                    }
                    match editor.readline(">> ")?.parse::<u8>() {
                        Ok(0) => {
                            break;
                        },
                        Ok(i) if compatible_resources.contains(&i) => {
                            if let Err(e) = explorer.play_sound(i, 0x10) {
                                println!("Failure playing sound: {}", e);
                            }
                        },
                        _ => {
                            println!("Invalid selection");
                        }
                    }
                }
            },
            Ok(2) => {
                loop {
                    let compatible_resources = explorer.list_resources_by_type(ResType::Music);
                    for res_id in compatible_resources.iter() {
                        println!("{}", res_id);
                    }
                    match editor.readline(">> ")?.parse::<u8>() {
                        Ok(0) => {
                            break;
                        },
                        Ok(i) if compatible_resources.contains(&i) => {
                            if let Err(e) = explorer.play_song(i, Some(Duration::from_millis(100))) {
                                println!("Failure playing song: {}", e);
                            }
                        },
                        _ => {
                            println!("Invalid selection");
                        }
                    }
                }
            },
            Ok(0) => {
                break Ok(())
            },
            Ok(_) => {
                println!("Invalid selection");
            },
            Err(e) => {
                println!("Invalid selection");
            }
        }
    }
}