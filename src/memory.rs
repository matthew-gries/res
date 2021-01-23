
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
    /// Return (`Result<u8, &'static str>`): `Ok` if the read was successful, returning the byte read at the address.
    /// Otherwise, an `Err` is returned
    fn read(&mut self, addr: u16) -> Result<u8, &'static str>;

    /// Write a byte to the given address
    /// 
    /// Arguments: 
    /// * `addr` (`u16`): the address to write to
    /// * `byte` (`u8`): the byte to store at the address
    /// 
    /// Return (`Result<(), &'static str>`): `Ok` if the read was successful. Otherwise, an `Err` is returned
    fn write(&mut self, addr: u16, byte: u8) -> Result<(), &'static str>;

    /// Get the address in the memory map after adjusting for mirroring
    /// 
    /// Arguments: 
    /// * `addr` (`u16`): an address in the memory map
    /// 
    /// Return (`u16`): the address in memory that `addr` mirrors (or the same address if it is
    /// not a mirror)
    fn get_adjusted_address(addr: u16) -> u16;
}
