pub use crate::memory::Memory;
pub use crate::main_memory::MainMemory;
pub use crate::video_memory::VideoMemory;

use std::collections::HashMap;

const NAMETABLE_HEIGHT    : usize = 32;
const NAMETABLE_WIDTH     : usize = 30;
const PALETTE_HEIGHT      : usize = 8;
const PALETTE_WIDTH       : usize = 8;
const SYSTEM_PALETTE_ROW  : usize = 4;
const SYSTEM_PALETTE_COL  : usize = 16;
const IMAGE_PALETTE_SIZE  : usize = 16;
const SPRITE_PALETTE_SIZE : usize = 16;
const PATTERN_TILE_COUNT  : usize = 256;

const IMAGE_PALETTE_ADDR       : u16  = 0x3F00;
const SPRITE_PALETTE_ADDR      : u16  = 0x3F10;
const PATTERN_TABLE_ZERO_START : u16  = 0x0000;
const PATTERN_TABLE_ONE_START  : u16  = 0x0000;
const PATTERN_TABLE_LEN        : u16  = 0x1000;
const NAME_TABLE_ZERO_START    : u16  = 0x2000;
const NAME_TABLE_ONE_START     : u16  = 0x2400;
const NAME_TABLE_TWO_START     : u16  = 0x2800;
const NAME_TABLE_THREE_START   : u16  = 0x2C00;
const NAME_TABLE_LEN           : u16  = 0x3C0;
const ATTR_TABLE_ZERO_START    : u16  = 0x23C0;
const ATTR_TABLE_ONE_START     : u16  = 0x27C0;
const ATTR_TABLE_TWO_START     : u16  = 0x2BC0;
const ATTR_TABLE_THREE_START   : u16  = 0x2FC0;
const ATTR_TABLE_LEN           : u16  = 0x3C0;
const ATTR_TABLE_BLOCK_SIZE    : u16  = 4;

lazy_static! {
    // a mapping between the byte used in the image and sprite palettes and the colors the bytes
    // correspond to
    static ref SYSTEM_PALETTE: HashMap<u8, u32> = {
        let mut map = HashMap::new();

        map.insert(0x00, 0x484848);
        map.insert(0x01, 0x000858);
        map.insert(0x02, 0x000878);
        map.insert(0x03, 0x000870);
        map.insert(0x04, 0x380050);
        map.insert(0x05, 0x580010);
        map.insert(0x06, 0x580000);
        map.insert(0x07, 0x400000);
        map.insert(0x08, 0x100000);
        map.insert(0x09, 0x001800);
        map.insert(0x0A, 0x001E00);
        map.insert(0x0B, 0x00230A);
        map.insert(0x0C, 0x001820);
        map.insert(0x0D, 0x000000);
        map.insert(0x0E, 0x080808);
        map.insert(0x0F, 0x080808);
        map.insert(0x10, 0xA0A0A0);
        map.insert(0x11, 0x0048B8);
        map.insert(0x12, 0x0830E0);
        map.insert(0x13, 0x5818D8);
        map.insert(0x14, 0xA008A8);
        map.insert(0x15, 0xD00058);
        map.insert(0x16, 0xD01000);
        map.insert(0x17, 0xA02000);
        map.insert(0x18, 0x604000);
        map.insert(0x19, 0x085800);
        map.insert(0x1A, 0x006800);
        map.insert(0x1B, 0x006810);
        map.insert(0x1C, 0x006070);
        map.insert(0x1D, 0x080808);
        map.insert(0x1E, 0x080808);
        map.insert(0x1F, 0x080808);
        map.insert(0x20, 0xF8F8F8);
        map.insert(0x21, 0x20A0F8);
        map.insert(0x22, 0x5078F8);
        map.insert(0x23, 0x9868F8);
        map.insert(0x24, 0xF868F8);
        map.insert(0x25, 0xF870B0);
        map.insert(0x26, 0xF87068);
        map.insert(0x27, 0xF88018);
        map.insert(0x28, 0xC09800);
        map.insert(0x29, 0x70B000);
        map.insert(0x2A, 0x28C020);
        map.insert(0x2B, 0x00C870);
        map.insert(0x2C, 0x00C0D0);
        map.insert(0x2D, 0x282828);
        map.insert(0x2E, 0x080808);
        map.insert(0x2F, 0x080808);
        map.insert(0x30, 0xF8F8F8);
        map.insert(0x31, 0xA0D8F8);
        map.insert(0x32, 0xB0C0F8);
        map.insert(0x33, 0xD0B0F8);
        map.insert(0x34, 0xF8C0F8);
        map.insert(0x35, 0xF8C0E0);
        map.insert(0x36, 0xF8C0C0);
        map.insert(0x37, 0xF8C8A0);
        map.insert(0x38, 0xE8D888);
        map.insert(0x39, 0xC8E090);
        map.insert(0x3A, 0xA8E8A0);
        map.insert(0x3B, 0x90E8C8);
        map.insert(0x3C, 0x90E0E8);
        map.insert(0x3D, 0xA8A8A8);
        map.insert(0x3E, 0x080808);
        map.insert(0x3F, 0x080808);

        map
    };
}

/// Representation of the different pattern tables in the PPU memory map.
#[derive(Copy, Clone, Debug)]
pub enum PatternTableSegment {
    Zero,
    One
}

impl PatternTableSegment {
    /// Get the start (inclusive) index of the pattern table
    pub fn start(self) -> u16 {
        match self {
            PatternTableSegment::Zero => PATTERN_TABLE_ZERO_START,
            PatternTableSegment::One  => PATTERN_TABLE_ONE_START
        }
    }
}

/// Representation of the different name tables in the PPU memory map.
#[derive(Copy, Clone, Debug)]
pub enum NameTableSegment {
    Zero,
    One,
    Two,
    Three
}

impl NameTableSegment {
    /// Get the start (inclusive) index of the name table. 
    pub fn start(self) -> u16 {
        match self {
            NameTableSegment::Zero  => NAME_TABLE_ZERO_START,
            NameTableSegment::One   => NAME_TABLE_ONE_START, 
            NameTableSegment::Two   => NAME_TABLE_TWO_START, 
            NameTableSegment::Three => NAME_TABLE_THREE_START
        }
    }
}

/// Representation of the different attribute tables in the PPU memory map.
#[derive(Copy, Clone, Debug)]
pub enum AttributeTableSegment {
    Zero,
    One,
    Two,
    Three
}

impl AttributeTableSegment {
    /// Get the start (inclusive) index of the name table. 
    pub fn start(self) -> u16 {
        match self {
            AttributeTableSegment::Zero  => ATTR_TABLE_ZERO_START,
            AttributeTableSegment::One   => ATTR_TABLE_ONE_START, 
            AttributeTableSegment::Two   => ATTR_TABLE_TWO_START, 
            AttributeTableSegment::Three => ATTR_TABLE_THREE_START
        }
    }
}

/// An 8x8 pixel pattern tile. Each value in this table should be 0-3, inclusive.
type Pattern = [[u8; PALETTE_WIDTH]; PALETTE_HEIGHT];

/// A list of patterns for a page in memory (either 0x0000 - 0x1000 or 0x1000 - 0x2000).
/// Each pattern is an 8x8 table created from every 16 bytes in the pattern table.
/// Thus there are 0x1000 / 16 = 256 individual tiles in the table.
///
/// The indices of this table are used by the name table to receive a tile.
type PatternTable = [Pattern; PATTERN_TILE_COUNT];

/// The 32x30 name table of pattern tiles. Each entry in this table is the index of the tile to use
/// in the pattern table.
type NameTable = [[Pattern; NAMETABLE_WIDTH]; NAMETABLE_HEIGHT];

/// Representation of a 32 x 30 table of palette table indices generated by the attribute table
/// Each element of this table corresponds to the upper 2 bits of a tile used to identify which
/// palette to use for the given tile.
type AttributeTable = [[u8; NAMETABLE_WIDTH]; NAMETABLE_HEIGHT];

/// An attribute table that has been flattened to 1 dimension.
type AttributeTableFlatten = [u8; NAMETABLE_WIDTH * NAMETABLE_HEIGHT];

/// Image and sprite palettes
type ImagePalette  = [u8; IMAGE_PALETTE_SIZE];
type SpritePalette = [u8; SPRITE_PALETTE_SIZE];

/// Unflatten an attribute table.
fn unflatten_attribute_table(table: AttributeTableFlatten) -> AttributeTable {

    let mut attr_table = [[0; NAMETABLE_WIDTH]; NAMETABLE_HEIGHT];

    let mut idx = 0;

    for i in 0..NAMETABLE_HEIGHT {
        for j in 0..NAMETABLE_WIDTH {
            attr_table[i][j] = table[idx];
            idx += 1;
        }
    }

    attr_table
}

/// Get the attribute table containing the upper bits of the name table values, denoting the palette to use for each tile.
fn get_attribute_table(ppu_mem: &mut VideoMemory, attribute_page: AttributeTableSegment) -> AttributeTable {

    // helper func to fill an attribute table with a given value
    let fill_attr_table = |start_idx: usize, val: u8, table: &mut AttributeTableFlatten| {
        for i in 0..(ATTR_TABLE_BLOCK_SIZE as usize) {
            table[start_idx + i] = val;
        }
    };

    let mut attr_table = [0; NAMETABLE_WIDTH * NAMETABLE_HEIGHT];

    let start = attribute_page.start();
    let end = start + ATTR_TABLE_LEN;

    for i in start..end {
        let block: u8 = ppu_mem.read(i as u16).unwrap();
        let square_0_bits = block & 0x3;
        let square_1_bits = block & 0xC;
        let square_2_bits = block & 0x30;
        let square_3_bits = block & 0xC0;

        let attr_table_idx: usize = (i as usize) * 16;

        fill_attr_table(attr_table_idx, square_0_bits, &mut attr_table);
        fill_attr_table(attr_table_idx + 4, square_1_bits, &mut attr_table);
        fill_attr_table(attr_table_idx + 8, square_2_bits, &mut attr_table);
        fill_attr_table(attr_table_idx + 12, square_3_bits, &mut attr_table);
    }

    unflatten_attribute_table(attr_table)
}

/// Generate a 32x30 tile name table, containing the indices corresponding to pattern tiles in the pattern table. This is before the attribute table values are
/// applied to get palettes for each entry.
fn generate_background_patterns(ppu_mem: &mut VideoMemory, pattern_page: PatternTableSegment, nametable_page: NameTableSegment) -> NameTable {
    let patterns = generate_pattern_tiles(ppu_mem, pattern_page);
    let mut background = [[[[0; PALETTE_WIDTH]; PALETTE_HEIGHT]; NAMETABLE_WIDTH]; NAMETABLE_HEIGHT];

    let start = nametable_page.start();
    let mut nametable_mem_idx = start;

    for i in 0..NAMETABLE_WIDTH {
        for j in 0..NAMETABLE_HEIGHT {
            let idx = ppu_mem.read(nametable_mem_idx).unwrap() as usize;
            background[i][j] = patterns[idx].clone();
            nametable_mem_idx += 1;
        }
    }

    background
}

/// Generate a single 8x8 pattern tile.
fn generate_tile(ppu_mem: &mut VideoMemory, index: usize) -> Pattern {

    if index % 16 != 0 {
        panic!("Index not divisible by 0xF");
    }
    
    let mut pattern = [[0; PALETTE_WIDTH]; PALETTE_HEIGHT];

    // for every row of the tile
    for i in index..(index + 8) {
        let left = ppu_mem.read(i as u16).unwrap();
        let right = ppu_mem.read((i as u16) + 8).unwrap();

        // for every column of the tile (i.e. bit of each number loaded from pattern table)
        for j in 0..8 {
            let left_bit = left & (1 << j);
            let right_bit = right & (1 << j);

            let val = (left_bit << 1) | right_bit;

            pattern[i - index][j] = val;
        }
    }

    pattern
}

/// Go through the pattern tables and return a list of pattern tiles, from 0x0000 to 0x2000
fn generate_pattern_tiles(ppu_mem: &mut VideoMemory, page: PatternTableSegment) -> PatternTable {

    let mut pattern_table = [[[0; PALETTE_WIDTH]; PALETTE_HEIGHT]; PATTERN_TILE_COUNT];
    let start = page.start();
    let end = start + PATTERN_TABLE_LEN;

    for (pattern_idx, table_idx) in (start..end).step_by(0x10).enumerate() {
        pattern_table[table_idx as usize] = generate_tile(ppu_mem, pattern_idx);
    }

    pattern_table
}

/// Return the image palette currently in memory.
fn get_image_palette(ppu_mem: &mut VideoMemory) -> ImagePalette {
    let mut palette = [0; IMAGE_PALETTE_SIZE];

    for i in 0..palette.len() {
        palette[i] = ppu_mem.read(IMAGE_PALETTE_ADDR + (i as u16)).unwrap();
    }

    palette
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_background() {

    }
}
