use crate::memory::Memory;
use crate::memory::MemorySegmentation;

pub const MAIN_MEMORY_MAP_ADDRESSABLE_RANGE: usize = 0x10000;

pub struct MainMemory {
    data: [u8; MAIN_MEMORY_MAP_ADDRESSABLE_RANGE]
}

pub enum MainMemorySegment {
    RAM,
    IORegisters(bool),
    ExpansionRom,
    SRAM,
    PRGROM,
}

impl MemorySegmentation<MainMemorySegment> for MainMemorySegment {
    fn get_segmentation(addr: u16) -> MainMemorySegment {
	if addr < 0x2000 {
	    MainMemorySegment::RAM
	} else if addr < 0x4000 {
	    MainMemorySegment::IORegisters(true)
	} else if addr < 0x4020 {
	    MainMemorySegment::IORegisters(false)
	} else if addr < 0x6000 {
	    MainMemorySegment::ExpansionRom
	} else if addr < 0x8000 {
	    MainMemorySegment::SRAM
	} else {
	    MainMemorySegment::PRGROM
	}
    }
}

impl MainMemory {

    pub fn new() -> Self {
        MainMemory{data: [0; MAIN_MEMORY_MAP_ADDRESSABLE_RANGE]}
    }

    pub fn check_if_page_crossed(addr1: u16, addr2: u16) -> bool {
        // check if the high bytes are different
        let addr1_high = addr1 & 0xFF00;
        let addr2_high = addr2 & 0xFF00;
        addr1_high != addr2_high
    }
}

impl Memory<MainMemorySegment> for MainMemory {

    fn get_adjusted_address(addr: u16) -> u16 {
	let segment = MainMemorySegment::get_segmentation(addr);

	match segment {
	    MainMemorySegment::RAM => addr % 0x0800,
	    MainMemorySegment::IORegisters(is_mirrored) => {
		if is_mirrored {
		    ((addr - 0x2000) % 0x8) + 0x2000
		} else {
		    addr
		}
	    },
	    MainMemorySegment::ExpansionRom
		| MainMemorySegment::SRAM
		| MainMemorySegment::PRGROM
		=> addr
	}
    }

    fn read(&self, addr: u16) -> u8 {
	let adjusted_addr = Self::get_adjusted_address(addr);
        self.data[adjusted_addr as usize]
    }

    fn write(&mut self, addr: u16, byte: u8) {

	let adjusted_addr = Self::get_adjusted_address(addr);
        self.data[adjusted_addr as usize] = byte;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_ram() {
	let mut data = [0; MAIN_MEMORY_MAP_ADDRESSABLE_RANGE];
	data[0x0] = 1;
	data[0x1] = 2;
	data[0x07FF] = 3;

	let memory = MainMemory{data};
	
	assert_eq!(memory.read(0x0), 1);
	assert_eq!(memory.read(0x0800), 1);
	assert_eq!(memory.read(0x1000), 1);
	assert_eq!(memory.read(0x1800), 1);

	assert_eq!(memory.read(0x1), 2);
	assert_eq!(memory.read(0x0801), 2);
	assert_eq!(memory.read(0x1001), 2);
	assert_eq!(memory.read(0x1801), 2);

	assert_eq!(memory.read(0x07FF), 3);
	assert_eq!(memory.read(0x0FFF), 3);
	assert_eq!(memory.read(0x17FF), 3);
	assert_eq!(memory.read(0x1FFF), 3);
    }

    #[test]
    fn write_ram() {
	let mut memory = MainMemory::new();

	memory.write(0x0, 1);
	assert_eq!(memory.read(0x0), 1);
	assert_eq!(memory.read(0x0800), 1);
	assert_eq!(memory.read(0x1000), 1);
	assert_eq!(memory.read(0x1800), 1);

	memory.write(0x1, 2);
	assert_eq!(memory.read(0x1), 2);
	assert_eq!(memory.read(0x0801), 2);
	assert_eq!(memory.read(0x1001), 2);
	assert_eq!(memory.read(0x1801), 2);

	memory.write(0x07FF, 3);
	assert_eq!(memory.read(0x07FF), 3);
	assert_eq!(memory.read(0x0FFF), 3);
	assert_eq!(memory.read(0x17FF), 3);
	assert_eq!(memory.read(0x1FFF), 3);
    }

    #[test]
    fn read_io_reg() {
	let mut data = [0; MAIN_MEMORY_MAP_ADDRESSABLE_RANGE];
	data[0x2000] = 1;
	data[0x2007] = 2;
	data[0x4000] = 3;
	data[0x401F] = 4;

	let memory = MainMemory{data};
	
	assert_eq!(memory.read(0x2000), 1);
	assert_eq!(memory.read(0x2008), 1);
	assert_eq!(memory.read(0x3FF8), 1);

	assert_eq!(memory.read(0x2007), 2);
	assert_eq!(memory.read(0x200F), 2);
	assert_eq!(memory.read(0x3FFF), 2);

	assert_eq!(memory.read(0x4000), 3);
	assert_eq!(memory.read(0x401F), 4);
    }

    #[test]
    fn write_io_reg() {
	let mut memory = MainMemory::new();

	memory.write(0x2000, 1);
	memory.write(0x2007, 2);
	memory.write(0x4000, 3);
	memory.write(0x401F, 4);

	assert_eq!(memory.read(0x2000), 1);
	assert_eq!(memory.read(0x2008), 1);
	assert_eq!(memory.read(0x3FF8), 1);

	assert_eq!(memory.read(0x2007), 2);
	assert_eq!(memory.read(0x200F), 2);
	assert_eq!(memory.read(0x3FFF), 2);

	assert_eq!(memory.read(0x4000), 3);
	assert_eq!(memory.read(0x401F), 4);
    }
}
