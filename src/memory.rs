
/// Trait to represent the way a memory map should be segmented
/// 
/// Types:
/// * `T`: The segmentation return type that represents how the memory is segmented
pub trait MemorySegmentation<T> {

    /// Return the segment of the memory map the given address lies in
    /// 
    /// Arguments: 
    /// * `addr` (`u16`): the address to read in memory
    /// 
    /// Return (`T`): the segmentation to return for the given memory address
    fn get_segmentation(addr: u16) -> T;
}

/// Trait representing a generic memory device that supports read and write operations, as well as
/// address mirroring
/// 
/// Types: 
/// * `T`: the memory segmentation scheme that this memory uses
pub trait Memory<T: MemorySegmentation<T>> {

    /// Read a byte from the given address
    /// 
    /// Arguments: 
    /// * `addr` (`u16`): the address to read from
    /// 
    /// Return (`u8`): the byte at the given address
    fn read(&mut self, addr: u16) -> u8;

    /// Write a byte to the given address
    /// 
    /// Arguments: 
    /// * `addr` (`u16`): the address to write to
    /// * `byte` (`u8`): the byte to store at the address
    /// 
    /// Panics: if a byte cannot be written to the given address (i.e. writing to a read only section
    /// of memory)
    fn write(&mut self, addr: u16, byte: u8);

    /// Get the address in the memory map after adjusting for mirroring
    /// 
    /// Arguments: 
    /// * `addr` (`u16`): an address in the memory map
    /// 
    /// Return (`u16`): the address in memory that `addr` mirrors (or the same address if it is
    /// not a mirror)
    fn get_adjusted_address(addr: u16) -> u16;
}
