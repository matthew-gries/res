
pub const MEMORY_MAP_ADDRESSABLE_RANGE: usize = 0x10000;

pub struct Memory {
    data: [u8; MEMORY_MAP_ADDRESSABLE_RANGE]
}

enum MemoryMapSegments {
    RAM(u16),
    IORegisters(Option<u16>),
    ExpansionRom,
    SRAM,
    PRGROM,
}

impl Memory {

    pub fn from_data_buffer(data: [u8; MEMORY_MAP_ADDRESSABLE_RANGE]) -> Self {
        Memory{data}
    }

    pub fn check_if_page_crossed(addr1: u16, addr2: u16) -> bool {
        // check if the high bytes are different
        let addr1_high = addr1 & 0xFF00;
        let addr2_high = addr2 & 0xFF00;
        addr1_high != addr2_high
    }

    fn get_mem_map_segment(addr: u16) -> MemoryMapSegments {
	if addr < 0x2000 {
	    MemoryMapSegments::RAM(0x0800)
	} else if addr < 0x4000 {
	    MemoryMapSegments::IORegisters(Some(0x0008))
	} else if addr < 0x4020 {
	    MemoryMapSegments::IORegisters(None)
	} else if addr < 0x6000 {
	    MemoryMapSegments::ExpansionRom
	} else if addr < 0x8000 {
	    MemoryMapSegments::SRAM
	} else {
	    MemoryMapSegments::PRGROM
	}
    }

    fn get_address(addr: u16, segment: MemoryMapSegments) -> u16 {
	
	match segment {
	    MemoryMapSegments::RAM(mask) => addr % mask,
	    MemoryMapSegments::IORegisters(optional_mask) => {
		if let Some(mask) = optional_mask {
		    (addr % mask) + 0x2000
		} else {
		    addr
		}
	    },
	    MemoryMapSegments::ExpansionRom
		| MemoryMapSegments::SRAM
		| MemoryMapSegments::PRGROM
		=> addr
	}
    }

    pub fn read(&self, addr: u16) -> u8 {
	let mem_map_segment = Self::get_mem_map_segment(addr);
	let adjusted_addr = Self::get_address(addr, mem_map_segment);
        self.data[adjusted_addr as usize]
    }

    pub fn write(&mut self, addr: u16, byte: u8) -> Result<(), &'static str> {

	let mem_map_segment = Self::get_mem_map_segment(addr);
	let adjusted_addr = match mem_map_segment {
	    MemoryMapSegments::ExpansionRom | MemoryMapSegments::PRGROM => {
		return Err("Attempted to write to Read Only memory!");
	    }
	    _ => Self::get_address(addr, mem_map_segment)
	};
        self.data[adjusted_addr as usize] = byte;
	Ok(())
    }
}
