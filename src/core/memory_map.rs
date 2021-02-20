use consts::*;

/// Representation of the CPU's memory map
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CPUMemoryMap {
    Ram,
    Io,
    ExpansionRom,
    Sram,
    PrgRom,
}

impl CPUMemoryMap {
    /// Get the CPU memory map segment from the given address
    ///
    /// Arguments:
    /// * `addr` (`u16`):  the address from which to get the memory map segment
    ///
    /// Returns (`CPUMemoryMap`): the CPU memory map segment
    pub fn from_addr(addr: u16) -> CPUMemoryMap {
        let addr = addr as usize;
        if addr <= RAM_END {
            CPUMemoryMap::Ram
        } else if addr <= IO_END_TWO {
            CPUMemoryMap::Io
        } else if addr <= EXPANSION_ROM_END {
            CPUMemoryMap::ExpansionRom
        } else if addr <= SRAM_END {
            CPUMemoryMap::Sram
        } else {
            CPUMemoryMap::PrgRom
        }
    }
}

pub mod consts {

    pub const TOTAL_MEMORY_SIZE: usize = 0x10000;
    pub const RAM_SIZE: usize = 0x0800;
    pub const RAM_START: usize = 0x0000;
    pub const RAM_END: usize = 0x1FFF;
    pub const IO_SIZE_ONE: usize = 0x8;
    pub const IO_START_ONE: usize = 0x2000;
    pub const IO_END_ONE: usize = 0x2007;
    pub const IO_SIZE_TWO: usize = 0x20;
    pub const IO_START_TWO: usize = 0x4000;
    pub const IO_END_TWO: usize = 0x401F;
    pub const EXPANSION_ROM_SIZE: usize = 0x1FE0;
    pub const EXPANSION_ROM_START: usize = 0x4020;
    pub const EXPANSION_ROM_END: usize = 0x5FFF;
    pub const SRAM_SIZE: usize = 0x2000;
    pub const SRAM_START: usize = 0x6000;
    pub const SRAM_END: usize = 0x7FFF;
    pub const PRG_ROM_SIZE: usize = 0x8000;
    pub const PRG_ROM_START: usize = 0x8000;
    pub const PRG_ROM_END: usize = 0xFFFF;
}
