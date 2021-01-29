use crate::core::memory::Memory;
use crate::core::main_memory::MainMemory;
use crate::core::video_memory::VideoMemory;

use std::collections::HashMap;

/// The number of PPU cycles per scanline
const PPU_CYCLES_PER_SCANLINE: i32 = 341;
/// The number of PPU scanlines
const PPU_SCANLINE_COUNT: i32 = 262;
/// The scanline to set VBlank on
const SET_VBLANK_ON_SCANLINE: i32 = 241;
/// The scanline to set VBlank off
const SET_VBLANK_OFF_SCANLINE: i32 = -1;
/// The cycle to change VBlank on
const CHANGE_VBLANK_CYCLE: i32 = 1;

/// The address in the system memory where the PPU reports its status
pub const PPU_STATUS_REGISTER_ADDR: u16 = 0x2002;

lazy_static! {
    // A mapping between the byte used in the image and sprite palettes and the colors the bytes
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

/// Representation of a picture processing unit (PPU) in the NES system
pub struct PPU {
    /// The current PPU clock cycle for the current scanline. Always between 0 and 340, inclusive
    cycle: i32,
    /// The current scanline. Always between -1 and 260, inclusive
    scanline: i32,
    /// Flag to determine if the PPU is in VBlank
    vblank: bool
}

impl PPU {

    /// Construct a new Ppu at cycle 0 and scanline -1. The PPU is not in VBlank by default
    /// 
    /// Return (`Ppu`): the new PPU
    pub fn new() -> Self {
        PPU {cycle: 0, scanline: -1, vblank: false}
    }

    /// Undergo a single PPU clock cycle, doing all necessary updates to the PPU
    /// 
    /// Arguments: 
    /// * `cpu_mem` (`MainMemory`): the CPU's memory
    pub fn ppu_cycle(&mut self, cpu_mem: &mut MainMemory) {

        self.update_cycle_and_scanline();
        self.update_vblank_if_needed();
        self.update_status_register(cpu_mem);
    }

    /// Update the scanline and scanline cycle
    fn update_cycle_and_scanline(&mut self) {

        if self.cycle == PPU_CYCLES_PER_SCANLINE - 1 {
            self.scanline = if self.scanline == PPU_SCANLINE_COUNT - 2 { -1 } else { self.scanline + 1 };
        }

        self.cycle = (self.cycle + 1) % PPU_CYCLES_PER_SCANLINE;
    }

    /// Set or unset the VBlank flag on specific scanlines and cycles
    fn update_vblank_if_needed(&mut self) {
        if self.scanline == SET_VBLANK_ON_SCANLINE && self.cycle == CHANGE_VBLANK_CYCLE {
            self.vblank = true;
        } else if self.scanline == SET_VBLANK_OFF_SCANLINE && self.cycle == CHANGE_VBLANK_CYCLE {
            self.vblank = false;
        }
    }

    /// Update the PPU status register at CPU address 0x2002
    fn update_status_register(&self, cpu_mem: &mut MainMemory) {

        let mut status: u8 = 0;

        if self.vblank {
            status |= 0x80;
        }

        cpu_mem.write(PPU_STATUS_REGISTER_ADDR, status);
    }
}