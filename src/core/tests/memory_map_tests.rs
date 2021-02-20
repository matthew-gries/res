use crate::core::memory_map::*;

#[test]
fn test_from_addr() {
    assert_eq!(CPUMemoryMap::from_addr(0x0), CPUMemoryMap::Ram);
    assert_eq!(CPUMemoryMap::from_addr(0x1FFF), CPUMemoryMap::Ram);
    assert_eq!(CPUMemoryMap::from_addr(0x2000), CPUMemoryMap::Io);
    assert_eq!(CPUMemoryMap::from_addr(0x401F), CPUMemoryMap::Io);
    assert_eq!(CPUMemoryMap::from_addr(0x4020), CPUMemoryMap::ExpansionRom);
    assert_eq!(CPUMemoryMap::from_addr(0x5FFF), CPUMemoryMap::ExpansionRom);
    assert_eq!(CPUMemoryMap::from_addr(0x6000), CPUMemoryMap::Sram);
    assert_eq!(CPUMemoryMap::from_addr(0x7FFF), CPUMemoryMap::Sram);
    assert_eq!(CPUMemoryMap::from_addr(0x8000), CPUMemoryMap::PrgRom);
    assert_eq!(CPUMemoryMap::from_addr(0xFFFF), CPUMemoryMap::PrgRom);
}
