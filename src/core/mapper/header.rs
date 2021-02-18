
pub const HEADER_SIZE: usize = 16;

pub enum FileHeader {
    INes([u8; HEADER_SIZE]),
    Nes2_0([u8; HEADER_SIZE]),
}

pub struct CommonINESInfo {
    pub mirroring: u8,
    pub contains_cartridge_battery_backed: bool, // at $6000-7FFF
    pub trainer: bool, // 512-byte at $7000-71FF
    pub ignore_mirroring_ctrl: bool, // instead provide 4-screen VRAM
    pub mapper: u8,
    pub vs_unisystem: bool,
    pub playchoice_10: bool,
}

pub struct INESInfo {
    pub nes_info: CommonINESInfo,
    // flag 8, not widely used
    pub prg_ram_size: u8, 
    // flag 9, not usually used
    pub tv_system: u8, // this is also set by flag 10, but flag 10 is not in spec so we use flag 9 
    // flag 10, not usually honored
    pub prg_ram: bool, // at $6000-7FFF
    pub bus_conflicts: bool, 
}

pub fn check_ines_format_name(header_buf: &[u8; HEADER_SIZE]) -> bool {
    let n = 0x4E;
    let e = 0x45;
    let s = 0x53;
    let eof = 0x1A;
    header_buf[0] == n && header_buf[1] == e && header_buf[2] == s && header_buf[3] == eof
}

pub fn check_nes_2_0_format(header_buf: &[u8; HEADER_SIZE]) -> bool {
    check_ines_format_name(header_buf) && (header_buf[7] & 0x0C == 0x08)
}

pub fn generate_i_nes_info(header: &[u8; HEADER_SIZE]) -> INESInfo {
    let mirroring = header[6] & 0x1;
    let contains_cartridge_battery_backed = (header[6] & 0x2) >> 1 != 0;
    let trainer = (header[6] & 0x4) >> 2 != 0;
    let ignore_mirroring_ctrl = (header[6] & 0x8) >> 3 != 0;
    let mapper_low = (header[6] & 0xF0) >> 4;
    let mapper_high = header[7] & 0xF0;
    let mapper = mapper_high | mapper_low;
    let vs_unisystem = (header[7] & 0x1) != 0;
    let playchoice_10 = (header[7] & 0x2) >> 1 != 0;
    let nes_info = CommonINESInfo{
        mirroring,
        contains_cartridge_battery_backed,
        trainer,
        ignore_mirroring_ctrl,
        mapper,
        vs_unisystem,
        playchoice_10,
    };

    let prg_ram_size = header[8];

    let tv_system = header[9] & 0x1;
    let prg_ram = (header[10] & 0x10) >> 4 != 0;
    let bus_conflicts = (header[10] & 0x20) >> 5 != 0;

    INESInfo{nes_info, prg_ram_size, tv_system, prg_ram, bus_conflicts}
}
