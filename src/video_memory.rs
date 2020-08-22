use crate::memory::Memory;
use crate::memory::MemorySegmentation;

pub const VIDEO_MEMORY_MAP_ADDRESSABLE_RANGE: usize = 0x10000;

pub struct VideoMemory {
    data: [u8; VIDEO_MEMORY_MAP_ADDRESSABLE_RANGE]
}

pub enum VideoMemorySegment {
    PatternTable,
    NameTable(bool),
    Palettes(bool),
}

impl MemorySegmentation<VideoMemorySegment> for VideoMemorySegment {
    
    fn get_segmentation(addr: u16) -> VideoMemorySegment {
	let addr = addr % 0x4000;
	if addr < 0x2000 {
	    VideoMemorySegment::PatternTable
	} else if addr < 0x3F00 {
	    if addr < 0x3000 {
		VideoMemorySegment::NameTable(false)
	    } else {
		VideoMemorySegment::NameTable(true)
	    }
	} else {
	    if addr < 0x3F20 {
		VideoMemorySegment::Palettes(false)
	    } else {
		VideoMemorySegment::Palettes(true)
	    }
	}
    }
}

impl VideoMemory {
    
    pub fn new() -> Self {
	VideoMemory{data: [0; VIDEO_MEMORY_MAP_ADDRESSABLE_RANGE]}
    }
}

impl Memory<VideoMemorySegment> for VideoMemory {

    fn get_adjusted_address(addr: u16) -> u16 {
	let addr = addr % 0x4000;
	let segment = VideoMemorySegment::get_segmentation(addr);

	match segment {
	    VideoMemorySegment::PatternTable => addr,
	    VideoMemorySegment::NameTable(is_mirrored) => {
		if is_mirrored {
		    addr - 0x1000
		} else {
		    addr
		}
	    }
	    VideoMemorySegment::Palettes(is_mirrored) => {
		if is_mirrored {
		    ((addr - 0x3F00) % 0x20) + 0x3F00
		} else {
		    addr
		}
	    }
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
    fn read_pattern_table() {
	let mut data = [0; VIDEO_MEMORY_MAP_ADDRESSABLE_RANGE];
	data[0x0] = 1;
	data[0x1FFF] = 2;
	let memory = VideoMemory{data};

	assert_eq!(memory.read(0x0), 1);
	assert_eq!(memory.read(0x4000), 1);
	assert_eq!(memory.read(0x1FFF), 2);
	assert_eq!(memory.read(0x5FFF), 2);
    }

    #[test]
    fn write_pattern_table() {
	
	let mut memory = VideoMemory::new();

	memory.write(0x0, 1);
	memory.write(0x1FFF, 2);
	memory.write(0x5FFE, 3);

	assert_eq!(memory.read(0x0), 1);
	assert_eq!(memory.read(0x4000), 1);
	assert_eq!(memory.read(0x1FFF), 2);
	assert_eq!(memory.read(0x5FFF), 2);
	assert_eq!(memory.read(0x1FFE), 3);
	assert_eq!(memory.read(0x5FFE), 3);
    }

    #[test]
    fn read_name_table() {
	let mut data = [0; VIDEO_MEMORY_MAP_ADDRESSABLE_RANGE];
	data[0x2000] = 1;
	data[0x2EFF] = 2;
	data[0x2FFF] = 3;
	let memory = VideoMemory{data};

	assert_eq!(memory.read(0x2000), 1);
	assert_eq!(memory.read(0x3000), 1);
	assert_eq!(memory.read(0x2EFF), 2);
	assert_eq!(memory.read(0x3EFF), 2);
	assert_eq!(memory.read(0x2FFF), 3);
	assert_eq!(memory.read(0x3FFF), 0);
    }

    #[test]
    fn write_name_table() {

	let mut memory = VideoMemory::new();

	memory.write(0x2000, 1);
	memory.write(0x2EFF, 2);
	memory.write(0x2FFF, 3);

	assert_eq!(memory.read(0x2000), 1);
	assert_eq!(memory.read(0x3000), 1);
	assert_eq!(memory.read(0x2EFF), 2);
	assert_eq!(memory.read(0x3EFF), 2);
	assert_eq!(memory.read(0x2FFF), 3);
	assert_eq!(memory.read(0x3FFF), 0);
    }

    #[test]
    fn read_palettes() {

	let mut data = [0; VIDEO_MEMORY_MAP_ADDRESSABLE_RANGE];

	data[0x3F00] = 1;
	data[0x3F1F] = 2;

	let memory = VideoMemory{data};

	assert_eq!(memory.read(0x3F00), 1);
	assert_eq!(memory.read(0x3F20), 1);
	assert_eq!(memory.read(0x3FE0), 1);
	assert_eq!(memory.read(0x3F1F), 2);
	assert_eq!(memory.read(0x3F3F), 2);
	assert_eq!(memory.read(0x3FFF), 2);
    }

    #[test]
    fn write_palettes() {

	let mut memory = VideoMemory::new();
	
	memory.write(0x3F00, 1);
	memory.write(0x3F1F, 2);

	assert_eq!(memory.read(0x3F00), 1);
	assert_eq!(memory.read(0x3F20), 1);
	assert_eq!(memory.read(0x3FE0), 1);
	assert_eq!(memory.read(0x3F1F), 2);
	assert_eq!(memory.read(0x3F3F), 2);
	assert_eq!(memory.read(0x3FFF), 2);
    }
}
