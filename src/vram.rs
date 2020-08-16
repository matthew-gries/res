
pub const VRAM_MAP_ADDRESSABLE_RANGE: usize = 0x10000;

pub struct VRam {
    data: [u8; VRAM_MAP_ADDRESSABLE_RANGE]
}

pub enum VramMemoryMap {
    PatternTable,
    NameTable(Option<u16>),
    Palettes(u16),
}

impl VRam {

    pub fn get_vram_segment(addr: u16) -> VramMemoryMap {
	let addr = addr % 0x4000;
	if addr < 0x2000 {
	    VramMemoryMap::PatternTable
	} else if addr < 0x3F00 {
	    VramMemoryMap::NameTable(0x2000)
	} else {
	    VramMemoryMap::
	}
    }
    
    pub fn new() -> Self {
	Vram{data: [0; VRAM_MAP_ADDRESSABLE_RANGE]}
    }

    pub fn read(&self, addr: u16) -> u8 {
	let vram_segment = Self::get_vram_segment(addr);
	let adjusted_addr = Self::get_address(addr, vram_segment);
	self.data[adjusted_addr as usize]
    }

}
