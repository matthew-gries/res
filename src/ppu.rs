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
