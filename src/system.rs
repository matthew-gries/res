use crate::cpu::CPU;
use crate::mapper;
use crate::mapper::Mapper;
use crate::memory::Memory;
use crate::main_memory::MainMemory;
use crate::main_memory::MAIN_MEMORY_MAP_ADDRESSABLE_RANGE;

use nes_system_error::NesSystemError;

use std::fs::File;
use std::io::prelude::*;
use std::u16;
use std::convert::TryFrom;

const HEADER_SIZE: usize = 16;

pub struct NesSystem {
    cpu: CPU,
    memory: MainMemory
}

enum FileHeader {
    INes([u8; HEADER_SIZE]),
    Nes2_0([u8; HEADER_SIZE]),
}

struct CommonINESInfo {
    pub mirroring: u8,
    pub contains_cartridge_battery_backed: bool, // at $6000-7FFF
    pub trainer: bool, // 512-byte at $7000-71FF
    pub ignore_mirroring_ctrl: bool, // instead provide 4-screen VRAM
    pub mapper: u8,
    pub vs_unisystem: bool,
    pub playchoice_10: bool,
}

struct INESInfo {
    pub nes_info: CommonINESInfo,
    // flag 8, not widely used
    pub prg_ram_size: u8, 
    // flag 9, not usually used
    pub tv_system: u8, // this is also set by flag 10, but flag 10 is not in spec so we use flag 9 
    // flag 10, not usually honored
    pub prg_ram: bool, // at $6000-7FFF
    pub bus_conflicts: bool, 
}

impl NesSystem {

    /// Reset the system
    pub fn reset(&mut self) {
        let mut wait = self.cpu.reset(&mut self.memory);
        while wait != 0 {
            wait -= 1;
        }
    }

    /// Start the system
    pub fn run(&mut self) {
        loop {
            self.cpu.instruction_cycle(&mut self.memory).unwrap();
        }
    }
    
    /// Load the rom specificed by the given file path.
    pub fn load_rom(path: &String) -> Result<NesSystem, NesSystemError> {
        let rom = File::open(path);

        let mut rom = match rom {
            Ok(x) => x,
            Err(err) => {return Err(NesSystemError::SystemIOError(err))},
        };
        
        let mut memory = MainMemory::new();

        let file_format = Self::find_file_header(&mut rom)?;

        match file_format {
            FileHeader::INes(header) => Self::i_nes_handler(&mut rom, &header, &mut memory)?,
            FileHeader::Nes2_0(header) => Self::nes_2_0_handler(&mut rom, &header, &mut memory)?
        }

        let cpu = CPU::new();
        Ok(NesSystem{cpu, memory})
    }
}

// helper functions
impl NesSystem {

    fn read_from_rom(rom: &mut File, buf: &mut [u8]) -> Result<(), NesSystemError> {

        if let Err(err) = rom.read(buf) {
            Err(NesSystemError::SystemIOError(err))
        } else {
            Ok(())
        }
    }

    fn check_ines_format_name(header_buf: &[u8; HEADER_SIZE]) -> bool {
        let n = 0x4E;
        let e = 0x45;
        let s = 0x53;
        let eof = 0x1A;
        header_buf[0] == n && header_buf[1] == e && header_buf[2] == s && header_buf[3] == eof
    }

    fn check_nes_2_0_format(header_buf: &[u8; HEADER_SIZE]) -> bool {
        Self::check_ines_format_name(header_buf) && (header_buf[7] & 0x0C == 0x08)
    }

    fn find_file_header(rom: &mut File) -> Result<FileHeader, NesSystemError> {
        // read first four bytes from header to check for 'NES' constant
        let mut header_buf: [u8; HEADER_SIZE] = [0; HEADER_SIZE];

        Self::read_from_rom(rom, &mut header_buf)?;

        // check for iNES format
        if Self::check_ines_format_name(&header_buf) {
            // check for NES 2.0 format
            if Self::check_nes_2_0_format(&header_buf) { 
                Ok(FileHeader::Nes2_0(header_buf))
            } else {
                Ok(FileHeader::INes(header_buf))
            }
        } else {
            Err(NesSystemError::UnknownFileFormat)
        }

    }

    fn generate_i_nes_info(header: &[u8; HEADER_SIZE]) -> INESInfo {
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

    fn i_nes_handler(rom: &mut File, header: &[u8; HEADER_SIZE], memory: &mut MainMemory) -> Result<(), NesSystemError> {

        // get the size of the PRG ROM
        let prg_rom_bytes: usize = header[4] as usize;
        let _chr_rom_bytes: usize = header[5] as usize; // if this value is 0, the board uses CHR-RAM
        
        let ines_info = Self::generate_i_nes_info(header);
        let map = mapper::INES_1_0_MAPPER_TABLE.get(&ines_info.nes_info.mapper);
        let map = match map {
            Some(m) => m,
            None => {return Err(NesSystemError::UnknownMapper(ines_info.nes_info.mapper))},
        };

        // load trainer if it exists (we don't really need this)
        let _trainer_buffer = {
            if ines_info.nes_info.trainer {
                let mut trainer_buf: [u8; 512] = [0; 512];
                Self::read_from_rom(rom, &mut trainer_buf)?;
                Some(trainer_buf)
            } else {
                None
            }
        };
        
        // for now, just load in the program rom
        let prg_start = map.cpu_rom_bank_start as usize;
        let prg_end = (map.cpu_rom_bank_end as usize) + 1;

        let prg_rom_size = 16384 * prg_rom_bytes;

        // dynamically allocate buffer
        let mut prg_rom: Vec<u8> = vec![0; prg_rom_size];

        Self::read_from_rom(rom, &mut prg_rom[..])?;

        let mut data: [u8; MAIN_MEMORY_MAP_ADDRESSABLE_RANGE]
            = [0; MAIN_MEMORY_MAP_ADDRESSABLE_RANGE];
        
        for i in 0..(prg_end-prg_start) {

            let start_offset = prg_start + i;
            let write_address = match u16::try_from(start_offset) {
                Err(_) => {return Err(NesSystemError::MemoryIOError("Address overflow when loading PRG_ROM"))},
                Ok(x) => x
            };
                
            data[write_address as usize] = prg_rom[i];
        }

        // TODO: clean up this hack
        *memory = MainMemory::from(data);

        Ok(())
    }

    fn nes_2_0_handler(rom: &mut File, header: &[u8; HEADER_SIZE], memory: &mut MainMemory) -> Result<(), NesSystemError> {
        
        Ok(())
    }

}

mod nes_system_error {

    use std::error::Error;
    use std::fmt;

    #[derive(Debug)]
    pub enum NesSystemError {
        SystemIOError(std::io::Error),
        UnknownFileFormat,
        UnknownMapper(u8),
	    MemoryIOError(&'static str)
    }

    impl fmt::Display for NesSystemError {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                NesSystemError::SystemIOError(err) =>  err.fmt(f),
                NesSystemError::UnknownFileFormat => {
                    write!(f, "The file format of the given rom is unknown or unsupported.")
                },
                NesSystemError::UnknownMapper(mapper) => {
                    write!(f, "Mapper {} is unknown or unsupported.", mapper)
                },
		        NesSystemError::MemoryIOError(err) => write!(f, "{}", err)
            }
        }
    }

    impl Error for NesSystemError {}

    
}
