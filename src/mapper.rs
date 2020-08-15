use std::collections::HashMap;

pub struct Mapper {
    pub cpu_rom_bank_start: u16,
    pub cpu_rom_bank_end: u16,
}

impl Mapper {
    pub fn new(cpu_rom_bank_start: u16, cpu_rom_bank_end: u16) -> Self {
        Mapper{cpu_rom_bank_start, cpu_rom_bank_end}
    }
}

lazy_static! {
    pub static ref INES_1_0_MAPPER_TABLE: HashMap<u8, Mapper> = {
        let mut map = HashMap::new();

        map.insert(0, Mapper::new(0x8000, 0xFFFF));

        map
    };
}
