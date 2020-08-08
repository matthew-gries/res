
pub struct Memory;

impl Memory {

    pub fn check_if_page_crossed(addr1: u16, addr2: u16) -> bool {
        // check if the high bytes are different
        let addr1_high = (addr1 | 0xFF00) >> 8;
        let addr2_high = (addr2 | 0xFF00) >> 8;
        (addr1_high == addr2_high)
    }

    pub fn read(&self, addr: u16) -> u8 {
        0
    }
}
