
pub const MEMORY_MAP_ADDRESSABLE_RANGE: usize = 0x10000;

pub struct Memory {
    data: [u8; MEMORY_MAP_ADDRESSABLE_RANGE]
}

impl Memory {

    pub fn new() -> Self {
        Memory{data: [0; MEMORY_MAP_ADDRESSABLE_RANGE]}
    }

    pub fn from_data_buffer(data: [u8; MEMORY_MAP_ADDRESSABLE_RANGE]) -> Self {
        Memory{data}
    }

    pub fn check_if_page_crossed(addr1: u16, addr2: u16) -> bool {
        // check if the high bytes are different
        let addr1_high = addr1 & 0xFF00;
        let addr2_high = addr2 & 0xFF00;
        addr1_high != addr2_high
    }

    pub fn read(&self, addr: u16) -> u8 {
        self.data[addr as usize]
    }

    pub fn write(&mut self, addr: u16, byte: u8) {
        if addr >= 0x8000 {
            panic!("Attempted to write into ROM at address {}!", addr);
        }

        self.data[addr as usize] = byte
    }
}
