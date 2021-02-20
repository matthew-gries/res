use crate::core::memory_map::consts::*;
use crate::core::memory_map::CPUMemoryMap;

/// The CPU data bus that is responsible for allowing the CPU to read and write data to other components of the system
pub struct CPUBus {
    data: [u8; TOTAL_MEMORY_SIZE], // TODO: mapper
}

impl CPUBus {
    pub fn new() -> Self {
        CPUBus {
            data: [0; TOTAL_MEMORY_SIZE],
        }
    }

    /// Read a byte from the given address
    ///
    /// Arguments:
    /// * `addr` (`u16`): the address to read from
    ///
    /// Return (`u8`): the byte at the given address
    pub fn read(&mut self, addr: u16) -> u8 {
        self.data[Self::adjust_address(addr) as usize]
    }

    /// Write a byte to the given address
    ///
    /// Arguments:
    /// * `addr` (`u16`): the address to write to
    /// * `byte` (`u8`): the byte to store at the address
    ///
    /// Panics: if a byte cannot be written to the given address (i.e. writing to a read only section
    /// of memory)
    pub fn write(&mut self, addr: u16, data: u8) {
        self.data[Self::adjust_address(addr) as usize] = data;
    }

    /// Adjust an address for mirroring
    fn adjust_address(addr: u16) -> u16 {
        let mem_map_segment = CPUMemoryMap::from_addr(addr);
        let addr = addr as usize;
        let adj_addr = match mem_map_segment {
            CPUMemoryMap::Ram => addr % RAM_SIZE,
            CPUMemoryMap::Io => {
                if addr < IO_START_TWO {
                    (addr % IO_SIZE_ONE) + IO_START_ONE
                } else {
                    addr
                }
            }
            _ => addr,
        };
        adj_addr as u16
    }
}
