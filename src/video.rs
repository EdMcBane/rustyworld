use crate::video::PageSelector::{Back, Front, Page};
use minifb::{Window};
use byteorder::{BigEndian, ReadBytesExt};
use std::convert::TryInto;
use itertools::izip;
use std::rc::Rc;
use std::cell::RefCell;

pub const WIDTH: i16 = 320;
pub const HEIGHT: i16 = 200;
const TOT_PIXELS: usize = WIDTH as usize * HEIGHT as usize;

pub const DEFAULT_POLY_COLOR: u8 = 0xFF;

pub trait Video {
    fn load_bg(&mut self, page: &[u8]);
    fn copy_page(&mut self, src: PageSelector, dst: PageSelector, scroll_y: i16);
    fn fill_page(&mut self, page: PageSelector, color: u8);
    fn update_display(&mut self, page: PageSelector);
    fn set_palette(&mut self, palette: &[u8]);
    fn change_draw_target(&mut self, sel: PageSelector);
    fn draw_polygons(&mut self, polygon_buffer: &[u8], current_segment: &[u8], color: u8, zoom: u16, p: Point);
    fn draw_string(&mut self, id: u16, p: Point, color: u8);
}

pub trait VideoAdapter {
    fn update_display(&mut self, buffer: &[u8]);
    fn set_palette(&mut self, palette: &[u32]);
}

#[derive(Debug)]
struct Rect {
    left: i16,
    right: i16,
    top: i16,
    bottom: i16,
}

pub struct DefaultVideo<A: VideoAdapter> {
    pages: [Vec<u8>; 4],
    target_buffer: Option<usize>,
    // 0 = background from resource
    front_buffer: usize,
    back_buffer: usize,
    adapter: A,
}


impl<A: VideoAdapter> DefaultVideo<A> {
    pub fn new(adapter: A) -> DefaultVideo<A> {
        DefaultVideo {
            pages: [
                vec![0; TOT_PIXELS],
                vec![0; TOT_PIXELS],
                vec![0; TOT_PIXELS],
                vec![0; TOT_PIXELS]],
            target_buffer: None,
            front_buffer: 2,
            back_buffer: 1,
            adapter,
        }
    }

    fn get_page(&self, sel: PageSelector) -> usize {
        match sel {
            PageSelector::Page(page) if page < 4 => page,
            PageSelector::Back => self.back_buffer,
            PageSelector::Front => self.front_buffer,
            _ => panic!()
        }
    }

    fn draw_hierarchy(&mut self, polygon_buffer: &[u8], mut current_segment: &[u8], zoom: u16, mut p: Point) {
        p.x -= (current_segment.read_u8().unwrap() as i32 * zoom as i32 / 64) as i16;
        p.y -= (current_segment.read_u8().unwrap() as i32 * zoom as i32 / 64) as i16;
        let children = current_segment.read_u8().unwrap() + 1;
        for _ in 0..children {
            let mut offset = current_segment.read_u16::<BigEndian>().unwrap();
            let p = Point::new(
                p.x + (current_segment.read_u8().unwrap() as i32 * zoom as i32 / 64) as i16,
                p.y + (current_segment.read_u8().unwrap() as i32 * zoom as i32 / 64) as i16);
            let mut color: u8 = DEFAULT_POLY_COLOR;

            let bp = offset;
            offset &= 0x7FFF;
            if bp & 0x8000 != 0 {
                color = (current_segment.read_u8().unwrap() & 0x7F) as u8;
                current_segment.read_u8().unwrap(); // ugly but necessary
            }

            self.draw_polygons(polygon_buffer, &polygon_buffer[(offset as usize * 2)..], color, zoom, p);
        }
    }


    fn read_vertices(&self, mut vertex_data: &[u8], zoom: u16) -> Polygon {
        let w = vertex_data.read_u8().unwrap() as u16 * zoom as u16 / 64;
        let h = vertex_data.read_u8().unwrap() as u16 * zoom as u16 / 64;
        let num_points = vertex_data.read_u8().unwrap();
        const MAX_VERTICES_PER_POLYGON: u8 = 50;
        assert!((num_points % 2) == 0 && num_points < MAX_VERTICES_PER_POLYGON);

        let points = (0..num_points).into_iter().map(|_i| {
            let x = vertex_data.read_u8().unwrap() as u16 * zoom as u16 / 64;
            let y = vertex_data.read_u8().unwrap() as u16 * zoom as u16 / 64;
            Point::new(x as i16, y as i16)
        }).collect();
        Polygon {
            w,
            h,
            points,
        }
    }

    fn calc_step(p1: &Point, p2: &Point) -> (i32, u16) {
        let h: u16 = (p2.y - p1.y).try_into().unwrap();
        let div: i16 = 0x4000 / (h.max(1) as i16);
        let dx = (p2.x as i32 - p1.x as i32) * div as i32 * 4;
        (dx, h)
    }

    fn handle_pair_proper(mut cpt1: u32, mut cpt2: u32, y: i16, step1: i32, step2: i32, h: u16, mut callback: impl FnMut(i16, i16, i16)) -> Option<(u32, u32)> {
        cpt1 = (cpt1 & 0xFFFF0000) | 0x7FFF;
        cpt2 = (cpt2 & 0xFFFF0000) | 0x8000;

        if h == 0 {
            cpt1 = (cpt1 as i32 + step1) as u32;
            cpt2 = (cpt2 as i32 + step2) as u32;
        } else {
            for y in y..(y + h as i16).min(HEIGHT) {
                if y >= 0 {
                    let x1 = (cpt1 >> 16) as i16;
                    let x2 = (cpt2 >> 16) as i16;
                    if x1 < WIDTH && x2 >= 0 {
                        callback(x1.max(0), x2.min(WIDTH - 1), y);
                    }
                }
                cpt1 = (cpt1 as i32 + step1) as u32;
                cpt2 = (cpt2 as i32 + step2) as u32;
            }
        }
        if y < HEIGHT { Some((cpt1, cpt2)) } else { None }
    }

    pub fn fill_polygon(&mut self, polygon: Polygon, p: Point, color: u8) {
        let color = match color {
            0x10 => Color::PaletteShift,
            e if e < 0x10 => Color::Solid(color),
            _ => Color::Background,
        };

        if polygon.w == 0 && polygon.h == 1 && polygon.points.len() == 4 {
            self.draw_point(&p, color);
            return;
        }

        let bbox = polygon.to_bbox(p);
        if bbox.left >= WIDTH || bbox.right < 0 || bbox.top >= HEIGHT || bbox.bottom < 0 {
            return;
        }

        assert!(polygon.points.len() % 2 == 0);
        let revpoints = polygon.points.iter().rev().cloned().collect::<Vec<_>>();
        let forwards = polygon.points.windows(2);
        let backwards = revpoints.windows(2);

        let mut y = bbox.top;
        let mut cpt1 = ((bbox.left + polygon.points.last().unwrap().x) as u32) << 16;
        let mut cpt2 = ((bbox.left + polygon.points.first().unwrap().x) as u32) << 16;

        for (f, r) in forwards.zip(backwards).take(polygon.points.len() / 2 - 1) {
            let f1 = &f[0];
            let f2 = &f[1];
            let r1 = &r[0];
            let r2 = &r[1];
            let (step1, h1) = Self::calc_step(r1, r2);
            let (step2, h) = Self::calc_step(f1, f2);
            assert_eq!(h1, h);
            match Self::handle_pair_proper(cpt1, cpt2, y, step1, step2, h, |x1, x2, y| { self.draw_line(x1, x2, y, color) }) {
                Some(r) => {
                    (cpt1, cpt2) = r;
                    y += h as i16;
                }
                None => { break; }
            }
        }
    }

    fn draw_point(&mut self, p: &Point, color: Color) {
        if !(0..WIDTH).contains(&p.x) || !(0..HEIGHT).contains(&p.y) {
            return;
        }
        let offset = p.y as usize * WIDTH as usize + p.x as usize;
        match color {
            Color::Solid(color) => { self.pages[self.target_buffer.unwrap()][offset] = color; }
            Color::PaletteShift => { self.pages[self.target_buffer.unwrap()][offset] |= 0x8; }
            Color::Background => { self.pages[self.target_buffer.unwrap()][offset] = self.pages[0][offset]; }
        }
    }

    fn draw_line(&mut self, x1: i16, x2: i16, y: i16, color: Color) {
        let xmin = x1.min(x2) as usize;
        let xmax = x1.max(x2) as usize;
        let range = (y as usize * WIDTH as usize + xmin)..=(y as usize * WIDTH as usize + xmax);
        match color {
            Color::Solid(color) => { self.pages[self.target_buffer.unwrap()][range].iter_mut().for_each(|e| *e = color); }
            Color::PaletteShift => { self.pages[self.target_buffer.unwrap()][range].iter_mut().for_each(|e| *e |= 0x8); }
            Color::Background => {
                if 0 == self.target_buffer.unwrap() {
                    return; // It happens, and we can't handle it
                }
                let (l, r) = &mut self.pages.split_at_mut(self.target_buffer.unwrap());
                let bg = &l[0];
                let page = &mut r[0];
                page[range.clone()].copy_from_slice(&bg[range]);
            }
        }
    }
    fn draw_char(&mut self, char: u8, p: Point, color: u8) {
        let char_index = (char - b' ') as usize;
        let glyph = &crate::strings::FONT[(char_index * 8)..((char_index + 1) * 8)];
        let page = &mut self.pages[self.target_buffer.unwrap()];
        for (dy, &glyph_row) in glyph.iter().enumerate() {
            let offset = (p.y as usize + dy) * WIDTH as usize + p.x as usize * 8;
            let dest = &mut page[offset..(offset + 8)]; // TODO: col, not x
            let bits = std::iter::successors(Some(glyph_row), |ge| Some(ge << 1)).map(|ge| ge & 0x80 != 0).take(8);
            dest.iter_mut().zip(bits).for_each(|(e, b)| if b { *e = color });
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Color {
    Solid(u8),
    Background,
    PaletteShift,
}

#[derive(Debug, Clone)]
pub struct Point {
    x: i16,
    y: i16,
}

impl Point {
    pub fn new(x: i16, y: i16) -> Point {
        Point { x, y }
    }
}

#[derive(Debug)]
pub struct Polygon {
    w: u16,
    h: u16,
    points: Vec<Point>,
}

impl Polygon {
    fn to_bbox(&self, Point { x, y }: Point) -> Rect {
        Rect {
            left: x - self.w as i16 / 2,
            right: x + self.w as i16 / 2,
            top: y - self.h as i16 / 2,
            bottom: y + self.h as i16 / 2,
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum PageSelector {
    Page(usize),
    // 0xFF
    Back,
    // 0xFE
    Front,
}

impl PageSelector {
    pub fn from(val: usize) -> PageSelector {
        match val {
            0xFF => Back,
            0xFE => Front,
            e => Page(e & 0x3),
        }
    }
}


impl<A: VideoAdapter> Video for DefaultVideo<A> {
    fn load_bg(&mut self, src: &[u8]) {
        let dst = &mut self.pages[0];
        for (i, (a, b, c, d)) in izip!(
            src[24000..].iter(),
            src[16000..].iter(),
            src[8000..].iter(),
            src[0..8000].iter()).enumerate() {
            for j in 0..8 {
                dst[i * 8 + j as usize] =
                    ((a << j & 0x80) >> 4)
                        | ((b << j & 0x80) >> 5)
                        | ((c << j & 0x80) >> 6)
                        | ((d << j & 0x80) >> 7);
            }
        }
    }


    fn copy_page(&mut self, src: PageSelector, dst: PageSelector, vscroll: i16) {
        if src == dst {
            return;
        }

        let src_id = self.get_page(src);
        let dst_id = self.get_page(dst);
        if src_id == dst_id {
            return;
        }
        let (src, dst) = if src_id < dst_id {
            let (l, r) = self.pages.split_at_mut(dst_id);
            (&l[src_id], &mut r[0])
        } else {
            let (l, r) = self.pages.split_at_mut(src_id);
            (&r[0], &mut l[dst_id])
        };

        assert!(vscroll.abs() <= 199);
        let h = (HEIGHT - vscroll.abs()) as usize;

        if vscroll < 0 {
            dst[..h * WIDTH as usize].copy_from_slice(&src[vscroll.abs() as usize * WIDTH as usize..]);
        } else {
            dst[vscroll.abs() as usize * WIDTH as usize..].copy_from_slice(&src[..h * WIDTH as usize]);
        }
    }

    fn fill_page(&mut self, page: PageSelector, color: u8) {
        let page = &mut self.pages[self.get_page(page)];
        page.iter_mut().for_each(|e| *e = color);
    }

    fn update_display(&mut self, page: PageSelector) {
        match page {
            PageSelector::Page(page) if page < 4 => { self.front_buffer = page }
            PageSelector::Back => { std::mem::swap(&mut self.front_buffer, &mut self.back_buffer) }
            PageSelector::Front => {}
            _ => panic!()
        };
        let frontbuffer = &self.pages[self.front_buffer];
        self.adapter.update_display(frontbuffer);
    }

    fn set_palette(&mut self, palette: &[u8]) {
        let palette_colors: Vec<u32> = palette.chunks_exact(2).map(|c| {
            let c1 = &c[0];
            let c2 = &c[1];
            let r = ((((c1 & 0x0F) << 2) | ((c1 & 0x0F) >> 2)) << 2) as u32;
            let g = ((((c2 & 0xF0) >> 2) | ((c2 & 0xF0) >> 6)) << 2) as u32;
            let b = ((((c2 & 0x0F) >> 2) | ((c2 & 0x0F) << 2)) << 2) as u32;
            0xff000000 | r << 16 | g << 8 | b
        }).collect();
        self.adapter.set_palette(&palette_colors);
    }

    fn change_draw_target(&mut self, sel: PageSelector) {
        self.target_buffer = Some(self.get_page(sel));
    }

    fn draw_polygons(&mut self, polygon_buffer: &[u8], mut current_segment: &[u8], mut color: u8, zoom: u16, p: Point) {
        let cmd = current_segment.read_u8().unwrap();
        match cmd {
            cmd if cmd >= 0xC0 => {
                // polygon
                if color & 0x80 != 0 {
                    color = cmd & 0x3F;
                }
                let polygon = self.read_vertices(current_segment, zoom);
                self.fill_polygon(polygon, p, color);
            }
            cmd if cmd & 0x3F == 2 => {
                // hierarchy
                self.draw_hierarchy(polygon_buffer, current_segment, zoom, p);
            }
            _ => {
                panic!("draw_polygons with unknown command: {:x}", cmd)
            }
        }
    }

    fn draw_string(&mut self, id: u16, p: Point, color: u8) {
        if let Some(message) = crate::strings::STRINGS.get(&id) {
            for (line_no, line) in message.split('\n').enumerate() {
                for (char_no, c) in line.bytes().enumerate() {
                    self.draw_char(c, Point::new(p.x + char_no as i16, p.y + line_no as i16 * 8), color);
                }
            }
        }
    }
}


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
