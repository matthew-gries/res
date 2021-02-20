pub struct CPUBus {
    data: [u8; 0x10000]
}

impl CPUBus {

    pub fn new() -> Self {
        CPUBus {
            data: [0; 0x10000]
        }
    }

    /// Read a byte from the given address
    ///
    /// Arguments:
    /// * `addr` (`u16`): the address to read from
    ///
    /// Return (`u8`): the byte at the given address
    pub fn read(&mut self, addr: u16) -> u8 {
        self.data[addr as usize]
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
        self.data[addr as usize] = data;
    }
}