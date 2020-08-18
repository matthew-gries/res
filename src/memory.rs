pub trait MemorySegmentation<T> {
    fn get_segmentation(addr: u16) -> T;
}

pub trait Memory<T: MemorySegmentation<T>> {
    fn read(&self, addr: u16) -> u8;

    fn write(&mut self, addr: u16, byte: u8) -> Result<(), &'static str>;

    fn get_adjusted_address(addr: u16) -> u16;
}
