use crate::memory::Memory;
use crate::memory::MemorySegmentation;

/// The range of all addresses in the memory map (this is the largest addressable address
/// + 1)
pub const MAIN_MEMORY_MAP_ADDRESSABLE_RANGE: usize = 0x10000;

/// Structure to represent the main memory of the NES.
pub struct MainMemory {
    data: [u8; MAIN_MEMORY_MAP_ADDRESSABLE_RANGE]
}

/// Enumeration of memory map segments in the main memory. IORegisters
/// contains segments that may or may not be mirrored, and therefore
/// contains a bool parameter to signify if an address is mirrored or
/// not.
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

    /// Constructs a new main memory object, with all values zeroed.
    pub fn new() -> Self {
        MainMemory{data: [0; MAIN_MEMORY_MAP_ADDRESSABLE_RANGE]}
    }

    /// Determine if the page of the two given addresses are the same. The page
    /// is defined as the high order byte.
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

    fn read(&mut self, addr: u16) -> Result<u8, &'static str> {

	let adjusted_addr = Self::get_adjusted_address(addr);
        let value = self.data[adjusted_addr as usize];
	if addr == 0x2002 {
	    // special conditions for reading PPU status.
	    //    - Bit 7 will be cleared
	    //    - Address latch of PPUSCROLL and PPUADDR is cleared
	    if let Err(e) = self.write(addr, value & 0x7F) {
		return Err(e);
	    }
	    // TODO
	}

	Ok(value)

    }

    fn write(&mut self, addr: u16, byte: u8) -> Result<(), &'static str> {

	match MainMemorySegment::get_segmentation(addr) {
	    MainMemorySegment::ExpansionRom | MainMemorySegment::PRGROM => {
		return Err("CPU attempted to write to read only memory!");
	    },
	    MainMemorySegment::IORegisters(_) => {
		if addr == 0x2002 {
		    return Err("CPU attempted to write to PPU status register!");
		}
	    }
	    _ => (),
	}

	let adjusted_addr = Self::get_adjusted_address(addr);
        self.data[adjusted_addr as usize] = byte;
	Ok(())
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

	let mut memory = MainMemory{data};
	
	assert_eq!(memory.read(0x0).unwrap(), 1);
	assert_eq!(memory.read(0x0800).unwrap(), 1);
	assert_eq!(memory.read(0x1000).unwrap(), 1);
	assert_eq!(memory.read(0x1800).unwrap(), 1);

	assert_eq!(memory.read(0x1).unwrap(), 2);
	assert_eq!(memory.read(0x0801).unwrap(), 2);
	assert_eq!(memory.read(0x1001).unwrap(), 2);
	assert_eq!(memory.read(0x1801).unwrap(), 2);

	assert_eq!(memory.read(0x07FF).unwrap(), 3);
	assert_eq!(memory.read(0x0FFF).unwrap(), 3);
	assert_eq!(memory.read(0x17FF).unwrap(), 3);
	assert_eq!(memory.read(0x1FFF).unwrap(), 3);
    }

    #[test]
    fn write_ram() {
	let mut memory = MainMemory::new();

	memory.write(0x0, 1).unwrap();
	assert_eq!(memory.read(0x0).unwrap(), 1);
	assert_eq!(memory.read(0x0800).unwrap(), 1);
	assert_eq!(memory.read(0x1000).unwrap(), 1);
	assert_eq!(memory.read(0x1800).unwrap(), 1);

	memory.write(0x1, 2).unwrap();
	assert_eq!(memory.read(0x1).unwrap(), 2);
	assert_eq!(memory.read(0x0801).unwrap(), 2);
	assert_eq!(memory.read(0x1001).unwrap(), 2);
	assert_eq!(memory.read(0x1801).unwrap(), 2);

	memory.write(0x07FF, 3).unwrap();
	assert_eq!(memory.read(0x07FF).unwrap(), 3);
	assert_eq!(memory.read(0x0FFF).unwrap(), 3);
	assert_eq!(memory.read(0x17FF).unwrap(), 3);
	assert_eq!(memory.read(0x1FFF).unwrap(), 3);
    }

    #[test]
    fn read_io_reg() {
	let mut data = [0; MAIN_MEMORY_MAP_ADDRESSABLE_RANGE];
	data[0x2000] = 1;
	data[0x2007] = 2;
	data[0x4000] = 3;
	data[0x401F] = 4;

	let mut memory = MainMemory{data};
	
	assert_eq!(memory.read(0x2000).unwrap(), 1);
	assert_eq!(memory.read(0x2008).unwrap(), 1);
	assert_eq!(memory.read(0x3FF8).unwrap(), 1);

	assert_eq!(memory.read(0x2007).unwrap(), 2);
	assert_eq!(memory.read(0x200F).unwrap(), 2);
	assert_eq!(memory.read(0x3FFF).unwrap(), 2);

	assert_eq!(memory.read(0x4000).unwrap(), 3);
	assert_eq!(memory.read(0x401F).unwrap(), 4);
    }

    #[test]
    fn write_io_reg() {
	let mut memory = MainMemory::new();

	memory.write(0x2000, 1).unwrap();
	memory.write(0x2007, 2).unwrap();
	memory.write(0x4000, 3).unwrap();
	memory.write(0x401F, 4).unwrap();

	assert_eq!(memory.read(0x2000).unwrap(), 1);
	assert_eq!(memory.read(0x2008).unwrap(), 1);
	assert_eq!(memory.read(0x3FF8).unwrap(), 1);

	assert_eq!(memory.read(0x2007).unwrap(), 2);
	assert_eq!(memory.read(0x200F).unwrap(), 2);
	assert_eq!(memory.read(0x3FFF).unwrap(), 2);

	assert_eq!(memory.read(0x4000).unwrap(), 3);
	assert_eq!(memory.read(0x401F).unwrap(), 4);
    }
}
