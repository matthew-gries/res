
const MEMORY_MAP_ADDRESSABLE_RANGE: usize = 0x10000;

pub struct Memory {
    data: [u8; MEMORY_MAP_ADDRESSABLE_RANGE]
}

impl Memory {

    pub fn new() -> Self {
        Memory{data: [0; MEMORY_MAP_ADDRESSABLE_RANGE]}
    }

    pub fn check_if_page_crossed(addr1: u16, addr2: u16) -> bool {
        // check if the high bytes are different
        let addr1_high = (addr1 | 0xFF00) >> 8;
        let addr2_high = (addr2 | 0xFF00) >> 8;
        (addr1_high == addr2_high)
    }

    pub fn read(&self, addr: u16) -> u8 {
        self.data[addr as usize]
    }

    pub fn write(&mut self, addr: u16, byte: u8) {
        self.data[addr as usize] = byte
    }
}
