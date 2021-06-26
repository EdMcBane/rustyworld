use arr_macro::arr;
use std::time::{Instant, Duration};
use crate::resources::ResourceManager;
use crate::video::{Video, PageSelector, Point, DEFAULT_POLY_COLOR};
use std::rc::Rc;
use byteorder::{ReadBytesExt, BigEndian};
use crate::audio::Audio;
use std::ops::Div;
use lazy_static::lazy_static;
use std::collections::{HashMap, HashSet};
use crate::music::Music;
use crate::input::InputDevice;
use crate::input::Input;
use std::borrow::BorrowMut;
use crate::vm::Segment::{Video1, Video2};

pub struct Vm<V, R, I> {
    speed_up: usize,
    state: VmState,
    peripherals: Peripherals<V, R, I>,

    chans: [Chan; 64],
    active_part: Part,
}

struct VmState {
    vars: [i16; 256],
    last_ts: Instant,
}

struct Peripherals<V, R, I> {
    resman: Rc<R>,
    audio: Box<dyn Audio>,
    music: Box<dyn Music>,
    video: V,
    input_device: I,
}

struct Chan {
    pc_offset: PcOffset,
    paused: bool,
    stack: Vec<u16>,
}

impl Default for Chan {
    fn default() -> Self {
        Chan {
            pc_offset: PcOffset::ThreadInactive,
            paused: false,
            stack: vec![],
        }
    }
}

const VM_SETVEC_INACTIVE: u16 = 0xFFFE;
const GAME_PART_BASE_IDX: u16 = 0x3E80;
const DEFAULT_ZOOM: u16 = 0x40;

#[derive(PartialEq, Debug)]
enum PcOffset {
    ThreadInactive,
    Offset(u16),
}

impl PcOffset {
    fn as_raw_offset(&self) -> u16 {
        match self {
            PcOffset::Offset(offset) => *offset,
            _ => { panic!("Not an offset"); }
        }
    }

    fn inc(&mut self, step: u16) -> usize {
        match self {
            PcOffset::Offset(offset) => {
                let prev = *offset as usize;
                *self = PcOffset::Offset(*offset + step);
                prev
            }
            PcOffset::ThreadInactive => { panic!("Not an offset"); }
        }
    }
}

enum ChanReq {
    Part(u16),
    PcOffset(usize, PcOffset),
    PauseState(usize, bool),
}

impl Chan {
    fn reset(&mut self) {
        self.pc_offset = PcOffset::ThreadInactive;
        self.paused = false;
    }

    fn jump_to(&mut self, pc: PcOffset) {
        self.pc_offset = pc;
    }

    fn next_u8(&mut self, code: &[u8]) -> u8 {
        (&code[self.pc_offset.inc(1)..]).read_u8().unwrap()
    }

    fn next_u16(&mut self, code: &[u8]) -> u16 {
        (&code[self.pc_offset.inc(2)..]).read_u16::<BigEndian>().unwrap()
    }
}

const VM_VAR_RANDOM_SEED: usize = 0x3C;
const VM_VAR_SCROLL_Y: usize = 0xF9;
const VM_VAR_MUS_MARK: usize = 0xF4;
// Set by player
const VM_VAR_PAUSE_SLICES: usize = 0xFF;

// const GAME_PART1: u16 = 0x3E80;
const GAME_PART2: u16 = 0x3E81;

fn map_input(inputs: HashSet<Input>) -> HashSet<VmInput> {
    inputs.into_iter().map(|i| match i {
        Input::UP => VmInput::UP,
        Input::DOWN => VmInput::DOWN,
        Input::LEFT => VmInput::LEFT,
        Input::RIGHT => VmInput::RIGHT,
        Input::BUTTON => VmInput::BUTTON,
    }).collect()
}

#[non_exhaustive]
#[derive(PartialEq, Eq, Hash)]
pub struct VmInput {
    mask: i16,
    value: i16,
}

impl VmInput {
    pub const RIGHT: VmInput = VmInput { mask: 1, value: 1 };
    pub const LEFT: VmInput = VmInput { mask: 2, value: -1 };
    pub const DOWN: VmInput = VmInput { mask: 4, value: 1 };
    pub const UP: VmInput = VmInput { mask: 8, value: -1 };
    pub const BUTTON: VmInput = VmInput { mask: 0x80, value: 1 };
}

const VM_VAR_HERO_POS_UP_DOWN: usize = 0xE5;
const VM_VAR_HERO_POS_LEFT_RIGHT: usize = 0xFC;
const VM_VAR_HERO_POS_JUMP: usize = 0xFB;
const VM_VAR_HERO_POS_MASK: usize = 0xFD;
const VM_VAR_HERO_ACTION: usize = 0xFA;
const VM_VAR_HERO_ACTION_MASK: usize = 0xFE;

struct Part {
    id: u16,
    code: Rc<Vec<u8>>,
    palette: Rc<Vec<u8>>,
    video1: Rc<Vec<u8>>,
    video2: Option<Rc<Vec<u8>>>,
}

impl Part {
    fn invalid() -> Part {
        Part {
            id: 0,
            code: Rc::new(vec![]),
            palette: Rc::new(vec![]),
            video1: Rc::new(vec![]),
            video2: None,
        }
    }
}

#[derive(Debug)]
enum JmpCond {
    Unconditional,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Lesser,
    LesserEqual,
}

#[derive(Debug)]
enum Instr {
    Move {
        reg: usize,
        src: Addressing,
    },
    Add {
        reg: usize,
        rhs: Addressing,
    },
    Call {
        target: PcOffset,
    },
    Ret,
    Yield,
    Jmp {
        cond: JmpCond,
        lhs_reg: usize,
        rhs: Addressing,
        target: PcOffset,
    },
    ChanExec {
        target_chan: usize,
        target: PcOffset,
    },
    DecJnz {
        reg: usize,
        target: PcOffset,
    },
    SetPalette {
        id: usize,
    },
    ChanReset {
        from_chan: usize,
        to_chan: usize,
        action: ChanAction,
    },
    SelectPage {
        page: PageSelector,
    },
    FillPage {
        page: PageSelector,
        color: u8,
    },
    CopyPage {
        src: PageSelector,
        dst: PageSelector,
        vscroll: bool,
    },
    UpdateDisplay {
        src: PageSelector,
    },
    Kill,
    DrawStr {
        id: u16,
        x: u8,
        y: u8,
        color: u8,
    },
    Sub {
        reg: usize,
        rhs: Addressing,
    },
    And {
        reg: usize,
        rhs: Addressing,
    },
    Or {
        reg: usize,
        rhs: Addressing,
    },
    Shl {
        reg: usize,
        rhs: Addressing,
    },
    Shr {
        reg: usize,
        rhs: Addressing,
    },
    PlaySound {
        id: u16,
        freq: u8,
        vol: u8,
        channel: u8,
    },
    UpdateMemList {
        id: u16,
    },
    PlayMusic {
        id: u16,
        delay: u16,
        pos: u8,
    },
    DrawPolyBg {
        offset: usize,
        x: i16,
        y: i16,
    },
    DrawPolySprite {
        offset: usize,
        x: Addressing,
        y: Addressing,
        segment: Segment,
        zoom: Addressing,
    },
}

#[derive(Debug)]
enum ChanAction {
    Pause,
    Resume,
    Reset,
}

#[derive(Debug)]
enum Segment {
    Video1,
    Video2,
}

#[derive(Debug)]
enum Addressing {
    Immediate(i16),
    Register(usize),
}

impl Addressing {
    fn resolve(self, regs: &[i16; 256]) -> i16 {
        match self {
            Addressing::Immediate(val) => val,
            Addressing::Register(reg) => regs[reg],
        }
    }
}

impl<V: Video, R: ResourceManager, I: InputDevice> Vm<V, R, I> {
    pub fn new(speed_up: usize, video: V, resman: Rc<R>, input_device: I, audio: Box<dyn Audio>, music: Box<dyn Music>) -> Vm<V, R, I> {
        let mut vm = Vm {
            speed_up,
            state: VmState {
                vars: [0i16; 256],
                last_ts: Instant::now(),
            },
            peripherals: Peripherals {
                resman,
                audio,
                music,
                video,
                input_device,
            },
            chans: arr![Default::default(); 64],
            active_part: Part::invalid(),
        };

        vm.state.vars[0x54] = 0x81;
        vm.state.vars[VM_VAR_RANDOM_SEED] = 0;

        // bypass protection
        vm.state.vars[0xBC] = 0x10;
        vm.state.vars[0xC6] = 0x80;
        vm.state.vars[0xF2] = 4000;
        vm.state.vars[0xDC] = 33;

        vm.state.vars[VM_VAR_MUS_MARK] = 0;
        vm
    }

    fn setup_part(&mut self, part_id: u16) {
        self.peripherals.music.stop();
        self.peripherals.audio.stop();
        self.state.vars[0xE4] = 0x14;

        let part = GAME_PARTS.get(&part_id).expect("no such part");
        assert_ne!(part_id, self.active_part.id);

        for res_id in &[part.palette_id, part.code_id, part.video1_id] {
            self.peripherals.resman.load_memory_entry(*res_id as u8);
        }
        for res_id in &part.video2_id {
            self.peripherals.resman.load_memory_entry(*res_id as u8);
            self.peripherals.video.load_bg(&self.peripherals.resman.resource(*res_id as u8).unwrap());
        }
        self.active_part = Part {
            id: part_id,
            palette: self.peripherals.resman.resource(part.palette_id as u8).unwrap(),
            code: self.peripherals.resman.resource(part.code_id as u8).unwrap(),
            video1: self.peripherals.resman.resource(part.video1_id as u8).unwrap(),
            video2: part.video2_id.map(|id| self.peripherals.resman.resource(id as u8).unwrap()),
        };

        self.chans.iter_mut().for_each(Chan::reset);
        self.chans[0].jump_to(PcOffset::Offset(0));
    }


    fn process_input(&mut self, inputs: HashSet<Input>) {
        let inputs = map_input(inputs);
        self.state.vars[VM_VAR_HERO_POS_LEFT_RIGHT] = inputs.get(&VmInput::RIGHT).or(inputs.get(&VmInput::LEFT)).map_or(0, |i| i.value);
        self.state.vars[VM_VAR_HERO_POS_UP_DOWN] = inputs.get(&VmInput::DOWN).or(inputs.get(&VmInput::UP)).map_or(0, |i| i.value);
        self.state.vars[VM_VAR_HERO_POS_JUMP] = self.state.vars[VM_VAR_HERO_POS_UP_DOWN];
        self.state.vars[VM_VAR_HERO_POS_MASK] = inputs.iter().map(|i| i.mask).fold(0, |a, m| a | m);
        self.state.vars[VM_VAR_HERO_ACTION] = inputs.get(&VmInput::BUTTON).map_or(0, |i| i.value);
        self.state.vars[VM_VAR_HERO_ACTION_MASK] = self.state.vars[VM_VAR_HERO_POS_MASK] | inputs.get(&VmInput::BUTTON).map_or(0, |i| i.mask); // TODO: check if copying the POS_MASK is required
    }

    pub fn run(&mut self) {
        self.setup_part(GAME_PART2);
        loop {
            // Process input
            self.process_input(self.peripherals.input_device.read_input());

            // Run channels
            let mut chan_reqs = vec![];
            for (chan_id, chan) in self.chans.iter_mut().enumerate() {
                if chan.paused || chan.pc_offset == PcOffset::ThreadInactive {
                    continue;
                }
                Vm::execute(&mut self.state, self.speed_up, chan, chan_id, &self.active_part, self.peripherals.borrow_mut(), &mut chan_reqs);
            }
            for req in chan_reqs {
                match req {
                    ChanReq::Part(part_id) => {
                        self.setup_part(part_id);
                        self.chans.iter_mut().for_each(Chan::reset);
                        self.chans[0].jump_to(PcOffset::Offset(0));
                    }
                    ChanReq::PcOffset(chan_id, offset) => {
                        self.chans[chan_id].pc_offset = offset;
                    }
                    ChanReq::PauseState(chan_id, state) => {
                        self.chans[chan_id].paused = state
                    }
                }
            }
        }
    }


    fn execute(state: &mut VmState, speed_up: usize, chan: &mut Chan, _chan_id: usize, part: &Part, peripherals: &mut Peripherals<V, R, I>, chan_reqs: &mut Vec<ChanReq>) {
        if let Some(val) = peripherals.music.latest_mark() {
            state.vars[VM_VAR_MUS_MARK] = val;
        }
        loop {
            let instr = <Vm<V, R, I>>::decode(chan, &part.code);
            eprintln!("Chan {} instr {:?}", _chan_id, instr);
            match instr {
                Instr::Move { reg, src } => {
                    state.vars[reg] = src.resolve(&state.vars)
                }
                Instr::Add { reg, rhs } => {
                    state.vars[reg] = state.vars[reg].overflowing_add(rhs.resolve(&state.vars)).0
                }
                Instr::Call { target } => {
                    chan.stack.push(chan.pc_offset.as_raw_offset());
                    chan.pc_offset = target;
                }
                Instr::Ret => {
                    chan.pc_offset = PcOffset::Offset(chan.stack.pop().expect("Stack underflow"));
                }
                Instr::Yield => break,
                Instr::ChanExec { target_chan, target } => {
                    chan_reqs.push(ChanReq::PcOffset(target_chan, target));
                }
                Instr::DecJnz { reg, target } => {
                    state.vars[reg] -= 1;
                    if state.vars[reg] != 0 {
                        chan.pc_offset = target;
                    }
                }
                Instr::Jmp { cond, lhs_reg, rhs, target } => {
                    let lhs = state.vars[lhs_reg];
                    let rhs = rhs.resolve(&state.vars);

                    if match cond {
                        JmpCond::Unconditional => true,
                        JmpCond::Equal => lhs == rhs,
                        JmpCond::NotEqual => lhs != rhs,
                        JmpCond::Greater => lhs > rhs,
                        JmpCond::GreaterEqual => lhs >= rhs,
                        JmpCond::Lesser => lhs < rhs,
                        JmpCond::LesserEqual => lhs <= rhs,
                    } {
                        chan.pc_offset = target;
                    }
                }
                Instr::SetPalette { id } => {
                    if id < 0x20 { // Sometimes we get palette 0x21
                        let palettes = &part.palette;
                        let palette = &palettes[(id * 32)..((id + 1) * 32)];
                        peripherals.video.set_palette(palette);
                    }
                }
                Instr::ChanReset { from_chan, to_chan, action } => {
                    for chan_id in from_chan..=to_chan {
                        match action {
                            ChanAction::Reset => chan_reqs.push(ChanReq::PcOffset(chan_id, PcOffset::ThreadInactive)),
                            ChanAction::Pause => chan_reqs.push(ChanReq::PauseState(chan_id, true)),
                            ChanAction::Resume => chan_reqs.push(ChanReq::PauseState(chan_id, false)),
                        }
                    }
                }
                Instr::SelectPage { page } => {
                    peripherals.video.change_draw_target(page);
                }
                Instr::FillPage { page, color } => {
                    peripherals.video.fill_page(page, color);
                }
                Instr::CopyPage { src, dst, vscroll } => {
                    let vscroll = if vscroll { state.vars[VM_VAR_SCROLL_Y] } else { 0 };
                    peripherals.video.copy_page(src, dst, vscroll);
                }
                Instr::UpdateDisplay { src } => {
                    // TODO: inp_handleSpecialKeys
                    let expected_delay = state.vars[VM_VAR_PAUSE_SLICES] as u32 * Duration::from_millis(20).div(speed_up as u32);
                    loop {
                        let elapsed = state.last_ts.elapsed();
                        if elapsed.ge(&expected_delay) {
                            break;
                        }
                        std::thread::sleep((expected_delay - elapsed).min(Duration::from_millis(5)));
                    }

                    state.last_ts += expected_delay;
                    state.vars[0xF7] = 0; // identify what this is used for
                    peripherals.video.update_display(src)
                }
                Instr::Kill => {
                    chan.pc_offset = PcOffset::ThreadInactive;
                    break;
                }
                Instr::DrawStr { id, x, y, color } => {
                    peripherals.video.draw_string(id, Point::new(x as i16, y as i16), color)
                }
                Instr::Sub { reg, rhs } => {
                    state.vars[reg] -= rhs.resolve(&state.vars);
                }
                Instr::And { reg, rhs } => {
                    state.vars[reg] = (state.vars[reg] as u16 & rhs.resolve(&state.vars) as u16) as i16;
                }
                Instr::Or { reg, rhs } => {
                    state.vars[reg] = (state.vars[reg] as u16 | rhs.resolve(&state.vars) as u16) as i16;
                }
                Instr::Shl { reg, rhs } => {
                    state.vars[reg] = ((state.vars[reg] as u16) << rhs.resolve(&state.vars) as u16) as i16;
                }
                Instr::Shr { reg, rhs } => {
                    state.vars[reg] = ((state.vars[reg] as u16) >> rhs.resolve(&state.vars) as u16) as i16;
                }
                Instr::PlaySound { id, channel, freq, vol } => {
                    if let Some(content) = peripherals.resman.resource(id as u8) {
                        peripherals.audio.play_sound(&content, freq, vol, channel);
                    }
                }
                Instr::UpdateMemList { id } => {
                    if id == 0 {
                        peripherals.music.stop();
                        peripherals.audio.stop();
                    } else if id >= GAME_PART_BASE_IDX {
                        chan_reqs.push(ChanReq::Part(id))
                    } else { // TODO: check resource_id range
                        if ETYPE_POLY_ANIM == peripherals.resman.load_memory_entry(id as u8).etype {
                            peripherals.video.load_bg(&(*peripherals.resman).resource(id as u8).unwrap());
                        }
                    }
                }
                Instr::PlayMusic { id, delay, pos } => {
                    if id != 0 {
                        peripherals.music.start(id, if delay != 0 { Some(delay) } else { None }, pos);
                    } else if delay != 0 {
                        peripherals.music.delay(delay);
                    } else {
                        peripherals.music.stop();
                    }
                }
                Instr::DrawPolyBg { offset, x, y } => {
                    let p = Point::new(x, y);
                    let cinematic_data = &part.video1;
                    peripherals.video.draw_polygons(&cinematic_data, &cinematic_data[offset..], DEFAULT_POLY_COLOR, DEFAULT_ZOOM, p);
                }
                Instr::DrawPolySprite { offset, x, y, segment, zoom } => {
                    let p = Point::new(x.resolve(&state.vars), y.resolve(&state.vars));
                    let poly_data = match segment {
                        Video1 => &part.video1,
                        Video2 => part.video2.as_ref().unwrap(),
                    };
                    peripherals.video.draw_polygons(&poly_data, &poly_data[offset..], DEFAULT_POLY_COLOR, zoom.resolve(&state.vars) as u16, p);
                }
            };
        }
    }

    fn decode(chan: &mut Chan, code: &[u8]) -> Instr {
        match chan.next_u8(code) {
            0x0 => {
                Instr::Move {
                    reg: chan.next_u8(code) as usize,
                    src: Addressing::Immediate(chan.next_u16(code) as i16),
                }
            }
            0x1 => {
                Instr::Move {
                    reg: chan.next_u8(code) as usize,
                    src: Addressing::Register(chan.next_u8(code) as usize),
                }
            }
            0x2 => {
                Instr::Add {
                    reg: chan.next_u8(code) as usize,
                    rhs: Addressing::Register(chan.next_u8(code) as usize),
                }
            }
            0x3 => {
                // TODO: handle gun sound bug, see original code
                Instr::Add {
                    reg: chan.next_u8(code) as usize,
                    rhs: Addressing::Immediate(chan.next_u16(code) as i16),
                }
            }
            0x4 => {
                Instr::Call {
                    target: PcOffset::Offset(chan.next_u16(code))
                }
            }
            0x5 => {
                Instr::Ret
            }
            0x6 => {
                Instr::Yield
            }
            0x7 => {
                Instr::Jmp {
                    cond: JmpCond::Unconditional,
                    lhs_reg: 0,
                    rhs: Addressing::Immediate(0),
                    target: PcOffset::Offset(chan.next_u16(code)),
                }
            }
            0x8 => {
                let chan_id = chan.next_u8(code) as usize;
                let pc_offset = match chan.next_u16(code) {
                    VM_SETVEC_INACTIVE => PcOffset::ThreadInactive,
                    offset => PcOffset::Offset(offset),
                };
                Instr::ChanExec {
                    target_chan: chan_id,
                    target: pc_offset,
                }
            }
            0x9 => {
                Instr::DecJnz {
                    reg: chan.next_u8(code) as usize,
                    target: PcOffset::Offset(chan.next_u16(code)),
                }
            }
            0xA => {
                let mode = chan.next_u8(code);
                let var_idx = chan.next_u8(code) as usize;
                Instr::Jmp {
                    cond: match mode & 0x7 {
                        0 => JmpCond::Equal,
                        1 => JmpCond::NotEqual,
                        2 => JmpCond::Greater,
                        3 => JmpCond::GreaterEqual,
                        4 => JmpCond::Lesser,
                        5 => JmpCond::LesserEqual,
                        _ => panic!("invalid condjump mode"),
                    },
                    lhs_reg: var_idx,
                    rhs: match mode & 0xC0 {
                        0x80 => Addressing::Register(chan.next_u8(code) as usize),
                        0x40 => Addressing::Immediate(chan.next_u16(code) as i16),
                        _ => Addressing::Immediate(chan.next_u8(code) as i16),
                    },
                    target: PcOffset::Offset(chan.next_u16(code)),
                }
            }
            0xB => {
                Instr::SetPalette {
                    id: (chan.next_u16(code) >> 8) as usize,
                }
            }
            0xC => {
                let from_idx = chan.next_u8(code) as usize;
                let to_idx = chan.next_u8(code) as usize;
                let state = chan.next_u8(code);
                let action = match state {
                    0 => ChanAction::Resume,
                    1 => ChanAction::Pause,
                    2 => ChanAction::Reset,
                    _ => { panic!("invalid reset thread state") }
                };
                Instr::ChanReset {
                    from_chan: from_idx,
                    to_chan: to_idx,
                    action,
                }
            }
            0xD => {
                Instr::SelectPage {
                    page: PageSelector::from(chan.next_u8(code) as usize)
                }
            }
            0xE => {
                Instr::FillPage {
                    page: PageSelector::from(chan.next_u8(code) as usize),
                    color: chan.next_u8(code),
                }
            }
            0xF => {
                let src = chan.next_u8(code);
                Instr::CopyPage {
                    src: PageSelector::from(src as usize),
                    dst: PageSelector::from(chan.next_u8(code) as usize),
                    vscroll: src < 0xFE && src & 0x80 != 0,
                }
            }
            0x10 => {
                Instr::UpdateDisplay {
                    src: PageSelector::from(chan.next_u8(code) as usize),
                }
            }
            0x11 => {
                Instr::Kill
            }
            0x12 => {
                Instr::DrawStr {
                    id: chan.next_u16(code),
                    x: chan.next_u8(code),
                    y: chan.next_u8(code),
                    color: chan.next_u8(code),
                }
            }
            0x13 => {
                Instr::Sub {
                    reg: chan.next_u8(code) as usize,
                    rhs: Addressing::Register(chan.next_u8(code) as usize),
                }
            }
            0x14 => {
                Instr::And {
                    reg: chan.next_u8(code) as usize,
                    rhs: Addressing::Immediate(chan.next_u16(code) as i16),
                }
            }
            0x15 => {
                Instr::Or {
                    reg: chan.next_u8(code) as usize,
                    rhs: Addressing::Immediate(chan.next_u16(code) as i16),
                }
            }
            0x16 => {
                Instr::Shl {
                    reg: chan.next_u8(code) as usize,
                    rhs: Addressing::Immediate(chan.next_u16(code) as i16),
                }
            }
            0x17 => {
                Instr::Shr {
                    reg: chan.next_u8(code) as usize,
                    rhs: Addressing::Immediate(chan.next_u16(code) as i16),
                }
            }
            0x18 => {
                Instr::PlaySound {
                    id: chan.next_u16(code),
                    freq: chan.next_u8(code),
                    vol: chan.next_u8(code),
                    channel: chan.next_u8(code),
                }
            }
            0x19 => {
                Instr::UpdateMemList {
                    id: chan.next_u16(code),
                }
            }
            0x1A => {
                Instr::PlayMusic {
                    id: chan.next_u16(code),
                    delay: chan.next_u16(code),
                    pos: chan.next_u8(code),
                }
            }
            opcode if opcode & 0x80 > 0 => {
                let offset = (((opcode as u16) << 8) | chan.next_u8(code) as u16).overflowing_mul(2).0 as usize; // TODO: clarify this
                let x = chan.next_u8(code) as i16;
                let y = chan.next_u8(code) as i16;
                let x = x + 0.max(y - 199);
                let y = y.min(199);
                Instr::DrawPolyBg {
                    offset,
                    x,
                    y,
                }
            }
            opcode if opcode & 0x40 > 0 => {
                let offset = chan.next_u16(code).overflowing_mul(2).0 as usize; // TODO: overflowing ?
                let x = chan.next_u8(code) as i16;
                let x = match (opcode >> 4) & 0x03 {
                    0 => Addressing::Immediate((x << 8) | chan.next_u8(code) as i16),
                    1 => Addressing::Register(x as usize),
                    2 => Addressing::Immediate(x),
                    3 => Addressing::Immediate(x + 0x100),
                    _ => panic!("Illegal x_submode")
                };

                let y = chan.next_u8(code) as i16;
                let y = match (opcode >> 2) & 0x03 {
                    0 => Addressing::Immediate((y << 8) | chan.next_u8(code) as i16),
                    1 => Addressing::Register(y as usize),
                    2..=3 => Addressing::Immediate(y),
                    _ => panic!("Illegal y_submode")
                };

                let (segment, zoom) = match opcode & 0x03 {
                    0 => (Video1, Addressing::Immediate(0x40)),
                    1 => (Video1, Addressing::Register(chan.next_u8(code) as usize)),
                    2 => (Video1, Addressing::Immediate(chan.next_u8(code) as i16)),
                    3 => (Video2, Addressing::Immediate(0x40)),
                    _ => panic!("Illegal z_submode")
                };
                Instr::DrawPolySprite {
                    offset,
                    x,
                    y,
                    segment,
                    zoom,
                }
            }
            _ => { panic!(); }
        }
    }
}


pub struct PartConfig {
    pub palette_id: u16,
    pub code_id: u16,
    pub video1_id: u16,
    pub video2_id: Option<u16>,
}

const ETYPE_POLY_ANIM: u8 = 2;
lazy_static! {
    static ref GAME_PARTS: HashMap<u16, PartConfig> = {
        let mut m = HashMap::new();
        m.insert(0x3E80,  PartConfig { palette_id: 0x14, code_id: 0x15, video1_id: 0x16, video2_id: None });
        m.insert(0x3E81,  PartConfig { palette_id: 0x17, code_id: 0x18, video1_id: 0x19, video2_id: None });
        m.insert(0x3E82,  PartConfig { palette_id: 0x1A, code_id: 0x1B, video1_id: 0x1C, video2_id: Some(0x11) });
        m.insert(0x3E83,  PartConfig { palette_id: 0x1D, code_id: 0x1E, video1_id: 0x1F, video2_id: Some(0x11) });
        m.insert(0x3E84,  PartConfig { palette_id: 0x20, code_id: 0x21, video1_id: 0x22, video2_id: Some(0x11) });
        m.insert(0x3E85,  PartConfig { palette_id: 0x23, code_id: 0x24, video1_id: 0x25, video2_id: None });
        m.insert(0x3E86,  PartConfig { palette_id: 0x26, code_id: 0x27, video1_id: 0x28, video2_id: Some(0x11) });
        m.insert(0x3E87,  PartConfig { palette_id: 0x29, code_id: 0x2A, video1_id: 0x2B, video2_id: Some(0x11) });
        m.insert(0x3E88,  PartConfig { palette_id: 0x7D, code_id: 0x7E, video1_id: 0x7F, video2_id: None });
        m.insert(0x3E89,  PartConfig { palette_id: 0x7D, code_id: 0x7E, video1_id: 0x7F, video2_id: None });
        m
    };
}

