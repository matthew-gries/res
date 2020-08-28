use crate::memory::Memory;
use crate::memory::MemorySegmentation;

/// The size of the addressanle range for the video memory (the largest addressable address
/// + 1)
pub const VIDEO_MEMORY_MAP_ADDRESSABLE_RANGE: usize = 0x10000;

/// Structure to represent the PPU's memory in the NES system.
pub struct VideoMemory {
    data: [u8; VIDEO_MEMORY_MAP_ADDRESSABLE_RANGE]
}

/// Enumeration of all segments in the memory map of the video memory. Both NameTable
/// and Palettes have sections of their segments that are not mirrored, and as such contain
/// a bool parameter that determines if the address in that segment should be mirrored.
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
    
    /// Construct a new video memory object
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

    fn read(&mut self, addr: u16) -> Result<u8, &'static str> {
		let adjusted_addr = Self::get_adjusted_address(addr);
		Ok(self.data[adjusted_addr as usize])
    }

    fn write(&mut self, addr: u16, byte: u8) -> Result<(), &'static str> {

	let adjusted_addr = Self::get_adjusted_address(addr);
        self.data[adjusted_addr as usize] = byte;
		Ok(())
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
		let mut memory = VideoMemory{data};

		assert_eq!(memory.read(0x0).unwrap(), 1);
		assert_eq!(memory.read(0x4000).unwrap(), 1);
		assert_eq!(memory.read(0x1FFF).unwrap(), 2);
		assert_eq!(memory.read(0x5FFF).unwrap(), 2);
    }

    #[test]
    fn write_pattern_table() {
		
		let mut memory = VideoMemory::new();

		memory.write(0x0, 1).unwrap();
		memory.write(0x1FFF, 2).unwrap();
		memory.write(0x5FFE, 3).unwrap();

		assert_eq!(memory.read(0x0).unwrap(), 1);
		assert_eq!(memory.read(0x4000).unwrap(), 1);
		assert_eq!(memory.read(0x1FFF).unwrap(), 2);
		assert_eq!(memory.read(0x5FFF).unwrap(), 2);
		assert_eq!(memory.read(0x1FFE).unwrap(), 3);
		assert_eq!(memory.read(0x5FFE).unwrap(), 3);
    }

    #[test]
    fn read_name_table() {
		let mut data = [0; VIDEO_MEMORY_MAP_ADDRESSABLE_RANGE];
		data[0x2000] = 1;
		data[0x2EFF] = 2;
		data[0x2FFF] = 3;
		let mut memory = VideoMemory{data};

		assert_eq!(memory.read(0x2000).unwrap(), 1);
		assert_eq!(memory.read(0x3000).unwrap(), 1);
		assert_eq!(memory.read(0x2EFF).unwrap(), 2);
		assert_eq!(memory.read(0x3EFF).unwrap(), 2);
		assert_eq!(memory.read(0x2FFF).unwrap(), 3);
		assert_eq!(memory.read(0x3FFF).unwrap(), 0);
    }

    #[test]
    fn write_name_table() {

		let mut memory = VideoMemory::new();

		memory.write(0x2000, 1).unwrap();
		memory.write(0x2EFF, 2).unwrap();
		memory.write(0x2FFF, 3).unwrap();

		assert_eq!(memory.read(0x2000).unwrap(), 1);
		assert_eq!(memory.read(0x3000).unwrap(), 1);
		assert_eq!(memory.read(0x2EFF).unwrap(), 2);
		assert_eq!(memory.read(0x3EFF).unwrap(), 2);
		assert_eq!(memory.read(0x2FFF).unwrap(), 3);
		assert_eq!(memory.read(0x3FFF).unwrap(), 0);
    }

    #[test]
    fn read_palettes() {

		let mut data = [0; VIDEO_MEMORY_MAP_ADDRESSABLE_RANGE];

		data[0x3F00] = 1;
		data[0x3F1F] = 2;

		let mut memory = VideoMemory{data};

		assert_eq!(memory.read(0x3F00).unwrap(), 1);
		assert_eq!(memory.read(0x3F20).unwrap(), 1);
		assert_eq!(memory.read(0x3FE0).unwrap(), 1);
		assert_eq!(memory.read(0x3F1F).unwrap(), 2);
		assert_eq!(memory.read(0x3F3F).unwrap(), 2);
		assert_eq!(memory.read(0x3FFF).unwrap(), 2);
    }

    #[test]
    fn write_palettes() {

		let mut memory = VideoMemory::new();
		
		memory.write(0x3F00, 1).unwrap();
		memory.write(0x3F1F, 2).unwrap();

		assert_eq!(memory.read(0x3F00).unwrap(), 1);
		assert_eq!(memory.read(0x3F20).unwrap(), 1);
		assert_eq!(memory.read(0x3FE0).unwrap(), 1);
		assert_eq!(memory.read(0x3F1F).unwrap(), 2);
		assert_eq!(memory.read(0x3F3F).unwrap(), 2);
		assert_eq!(memory.read(0x3FFF).unwrap(), 2);
    }
}
