use std::fs::File;
use std::io::prelude::*;
use std::error::Error;
use byteorder::{ByteOrder, BigEndian, ReadBytesExt};
use std::io::SeekFrom;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use num_enum::TryFromPrimitive;
use std::convert::TryInto;

pub struct Resource
{
    pub state: u8,
    pub etype: ResType,
    pub buf_ptr: u16,
    pub rank_num: u8,
    pub bank_id: u8,
    pub bank_offset: u32,
    pub packed_size: u32,
    pub size: u32,
}

#[repr(u8)]
#[derive(TryFromPrimitive, PartialEq, Debug)]
pub enum ResType {
    Sound = 0,
    Music = 1,
    PolyAnim = 2,
    // full screen video buffer, size=0x7D00
    Palette = 3,
    // palette (1024=vga + 1024=ega), size=2048
    Bytecode = 4,
    PolyCinematic = 5,
    Unknown = 6,
}

pub trait ResourceManager {
    fn load_memory_entry(&self, resource_id: u8) -> &Resource;
    fn resource(&self, id: u8) -> Option<Rc<Vec<u8>>>;
}

pub struct FileResourceManager {
    pub resources: Vec<Resource>,
    pub buffers: RefCell<HashMap<u8, Rc<Vec<u8>>>>,
}

#[derive(Debug)]
enum PackOp {
    Literal {
        len_bits: usize,
        bytes: usize,
    },
    Ref {
        offset_bits: usize,
        count_bits: usize,
        bytes: usize,
    },
}

impl PackOp {
    fn consume<T: Iterator<Item=u32>>(&self, src: &mut BitStream<T>, dst: &mut Vec<u8>) -> Result<(), Box<dyn Error>> {
        match self {
            PackOp::Literal { len_bits, bytes } => {
                let var_len = src.nextbits(*len_bits).ok_or("")?;
                for _ in 0..(bytes + var_len) {
                    dst.push(src.nextbits(8).ok_or("")? as u8);
                }
            }
            PackOp::Ref { offset_bits, count_bits, bytes } => {
                let var_len = src.nextbits(*count_bits).ok_or("")?;
                let offset = src.nextbits(*offset_bits).ok_or("")?;
                let tot_len = bytes + var_len;
                // No fancy memmove here, its RLE like
                for _ in 0..tot_len {
                    dst.push(dst[dst.len() - offset]);
                }
            }
        }
        Ok(())
    }
}

struct BitStream<I> {
    bits: u32,
    crc: u32,
    src: I,
}

fn from_bytes(bytes: &[u8], crc: u32) -> BitStream<impl Iterator<Item=u32> + '_> {
    let mut src = bytes.rchunks_exact(4)
        .map(BigEndian::read_u32);
    let first = src.next().unwrap();
    BitStream {
        bits: first,
        crc: crc ^ first,
        src,
    }
}

impl<I> BitStream<I> where I: Iterator<Item=u32> {
    fn nextbit(&mut self) -> Option<bool> {
        let mut res = (self.bits & 1) != 0;
        self.bits >>= 1;
        if self.bits == 0 {
            self.bits = self.src.next()?;
            self.crc ^= self.bits;
            res = (self.bits & 1) != 0;
            self.bits = (1 << 31) | (self.bits >> 1)
        }
        Some(res)
    }

    fn nextbits(&mut self, n: usize) -> Option<usize> {
        let mut bits = 0usize;
        for i in 0..n {
            bits |= (self.nextbit()? as usize) << (n - 1 - i);
        }
        Some(bits)
    }
}

impl<T: Iterator<Item=u32>> Iterator for BitStream<T> {
    type Item = bool;

    fn next(&mut self) -> Option<Self::Item> {
        self.nextbit()
    }
}


impl FileResourceManager {
    pub fn new() -> Result<FileResourceManager, Box<dyn Error>> {
        Ok(FileResourceManager {
            resources: index_resources()?,
            buffers: Default::default(),
        })
    }

    pub fn read_resource(&self, id: u8) -> Result<Vec<u8>, Box<dyn Error>> {
        let res = &self.resources[id as usize];
        let mut file = File::open(format!("data/BANK{:02X}", res.bank_id)).map_err(|e| format!("failed to open bank {:02X}: {}", res.bank_id, e))?;
        file.seek(SeekFrom::Start(res.bank_offset as u64))?;
        let mut buf: Vec<u8> = Vec::with_capacity(res.packed_size as usize);
        assert_eq!(res.packed_size as usize, file.take(res.packed_size as u64).read_to_end(&mut buf)?);
        if res.size != res.packed_size {
            Ok(FileResourceManager::unpack(buf, res.packed_size as usize, res.size as usize)?)
        } else {
            Ok(buf)
        }
    }

    fn unpack(buf: Vec<u8>, packed_size: usize, unpacked_size: usize) -> Result<Vec<u8>, Box<dyn Error>> {
        let mut out: Vec<u8> = Vec::with_capacity(unpacked_size);
        let mut src = from_bytes(&buf[..packed_size - 8], (&buf[packed_size - 8..]).read_u32::<BigEndian>()?);
        loop {
            let op = match src.nextbit() {
                Some(false) => match src.nextbit().ok_or("no data")? {
                    false => PackOp::Literal { len_bits: 3, bytes: 1 },
                    true => PackOp::Ref { offset_bits: 8, count_bits: 0, bytes: 2 },
                }
                Some(true) => match src.nextbits(2).ok_or("no data")? {
                    3 => PackOp::Literal { len_bits: 8, bytes: 9 },
                    2 => PackOp::Ref { offset_bits: 12, count_bits: 8, bytes: 1 },
                    1 => PackOp::Ref { offset_bits: 10, count_bits: 0, bytes: 4 },
                    0 => PackOp::Ref { offset_bits: 9, count_bits: 0, bytes: 3 },
                    _ => { panic!() }
                },
                None => { break; }
            };
            op.consume(&mut src, &mut out)?;
        }
        out.reverse(); // since we write in reverse
        if src.crc != 0 {
            return Err("Invalid crc".into());
        }
        Ok(out)
    }
}

impl ResourceManager for FileResourceManager {
    fn load_memory_entry(&self, resource_id: u8) -> &Resource {
        let res = &self.resources[resource_id as usize];
        let content = self.read_resource(resource_id).unwrap_or_else(|e| panic!("failed to load resource {}: {}", resource_id, e));
        let _ = self.buffers.borrow_mut().insert(resource_id, Rc::new(content));
        res
    }

    fn resource(&self, id: u8) -> Option<Rc<Vec<u8>>> {
        self.buffers.borrow().get(&id).cloned()
    }
}

fn index_resources() -> Result<Vec<Resource>, Box<dyn Error>> {
    let mut file = File::open("data/MEMLIST.BIN")?;
    let mut resources = vec![];
    while let Some(res) = parse_resource(&mut file)? {
        resources.push(res);
    }
    Ok(resources)
}

fn parse_resource(file: &mut File) -> Result<Option<Resource>, Box<dyn Error>> {
    let state = file.read_u8()?;
    let etype = file.read_u8()?;
    let buf_ptr = file.read_u16::<BigEndian>()?;
    file.seek(SeekFrom::Current(2))?;
    let rank_num = file.read_u8()?;
    let bank_id = file.read_u8()?;
    let bank_offset = file.read_u32::<BigEndian>()?;
    let packed_size = file.read_u32::<BigEndian>()?;
    let size = file.read_u32::<BigEndian>()?;
    if etype != 0xFF {
        Ok(Some(Resource {
            state,
            etype: etype.try_into()?,
            buf_ptr,
            rank_num,
            bank_id,
            bank_offset,
            packed_size,
            size
        }))
    } else {
        Ok(None)
    }
}


#[cfg(test)]
mod tests {
    use crate::resources::*;

    #[test]
    fn can_stream_bytes() {
        let bytes = vec![8u8, 48, 2u8, 1u8];
        let mut stream = from_bytes(&bytes, 0);
        assert_eq!(true, stream.nextbit().unwrap());
        assert_eq!(false, stream.nextbit().unwrap());
        assert_eq!(false, stream.nextbit().unwrap());
        assert_eq!(false, stream.nextbit().unwrap());
        assert_eq!(false, stream.nextbit().unwrap());
        assert_eq!(false, stream.nextbit().unwrap());
        assert_eq!(false, stream.nextbit().unwrap());
        assert_eq!(false, stream.nextbit().unwrap());

        assert_eq!(false, stream.nextbit().unwrap());
        assert_eq!(true, stream.nextbit().unwrap());
        assert_eq!(false, stream.nextbit().unwrap());
        assert_eq!(false, stream.nextbit().unwrap());
        assert_eq!(false, stream.nextbit().unwrap());
        assert_eq!(false, stream.nextbit().unwrap());
        assert_eq!(false, stream.nextbit().unwrap());
        assert_eq!(false, stream.nextbit().unwrap());
    }

    #[test]
    fn it_works2() {
        let src = vec![1u32, 2u32, 3u32];
        let mut stream = from_rev_u32s(src);
        assert_eq!(1, stream.by_ref().take(32).filter(|e| *e).count());
        assert_eq!(2, stream.by_ref().take(32).filter(|e| *e).count());
    }

    #[test]
    fn it_works3() {
        let src = vec![0x4, 0x7f80006];
        let mut stream = from_rev_u32s(src);
        assert_eq!(0, stream.by_ref().nextbit().unwrap() as u32);
        assert_eq!(0, stream.by_ref().nextbit().unwrap() as u32);
        assert_eq!(3, stream.by_ref().nextbits(3).unwrap() as u32);
        assert_eq!(0, stream.by_ref().nextbits(8).unwrap() as u32);
        assert_eq!(0, stream.by_ref().nextbits(8).unwrap() as u32);
        assert_eq!(255, stream.by_ref().nextbits(8).unwrap() as u32);
    }

    fn from_rev_u32s(u32s: Vec<u32>) -> BitStream<impl Iterator<Item=u32>> {
        let mut src = u32s.into_iter();
        BitStream {
            bits: src.next().unwrap(),
            crc: 0,
            src: src.into_iter(),
        }
    }
}