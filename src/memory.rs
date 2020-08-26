
/// Trait to represent the way a memory map should be segmented, with the specific segmentation
/// enumeration being represented by generic parameter T
pub trait MemorySegmentation<T> {

    /// Return the segment of the memory map the given address lies in.
    fn get_segmentation(addr: u16) -> T;
}

/// Trait representing a generic memory device that supports read and write operations, as well as
/// address mirroring
pub trait Memory<T: MemorySegmentation<T>> {

    /// Read a byte from the given address.
    fn read(&mut self, addr: u16) -> Result<u8, &'static str>;

    /// Write a byte to the given address.
    fn write(&mut self, addr: u16, byte: u8) -> Result<(), &'static str>;

    /// Get the address in the memory map after adjusting for mirroring.
    fn get_adjusted_address(addr: u16) -> u16;
}
