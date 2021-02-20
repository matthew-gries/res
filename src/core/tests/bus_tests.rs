use crate::core::bus::CPUBus;

#[test]
fn test_cpu_bus_mem_maps() {
    let mut bus = CPUBus::new();

    bus.write(0x0, 1);
    bus.write(0x801, 2);
    bus.write(0x1002, 3);
    bus.write(0x2000, 4);
    bus.write(0x4000, 5);

    assert_eq!(bus.read(0x0), 1);
    assert_eq!(bus.read(0x1), 2);
    assert_eq!(bus.read(0x2), 3);
    assert_eq!(bus.read(0x800), 1);
    assert_eq!(bus.read(0x1000), 1);
    assert_eq!(bus.read(0x801), 2);
    assert_eq!(bus.read(0x802), 3);
    assert_eq!(bus.read(0x1002), 3);
    assert_eq!(bus.read(0x2000), 4);
    assert_eq!(bus.read(0x2008), 4);
    assert_eq!(bus.read(0x4000), 5);
}
