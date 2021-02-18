use crate::core::cpu::CPU;
use crate::core::ppu::PPU;
use crate::core::main_memory::MainMemory;
use crate::core::main_memory::MAIN_MEMORY_MAP_ADDRESSABLE_RANGE;
use crate::core::mapper::header::{HEADER_SIZE, FileHeader};
use crate::core::mapper::mapper;
use crate::core::mapper::header;

use nes_system_error::NesSystemError;

use std::fs::File;
use std::io::prelude::*;
use std::u16;
use std::convert::TryFrom;

pub struct NesSystem {
    cpu: CPU,
    ppu: PPU,
    memory: MainMemory,
    // TODO: is this synced to cpu or ppu? for now just do PPU
    sys_clock: u64
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

            if self.sys_clock % 3 == 0 {
                self.cpu.instruction_cycle(&mut self.memory);
            }

            self.ppu.ppu_cycle(&mut self.memory);

            self.sys_clock += 1;
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
        let ppu = PPU::new();
        Ok(NesSystem{cpu, ppu, memory, sys_clock: 0})
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

    fn find_file_header(rom: &mut File) -> Result<FileHeader, NesSystemError> {
        // read first four bytes from header to check for 'NES' constant
        let mut header_buf: [u8; HEADER_SIZE] = [0; HEADER_SIZE];

        Self::read_from_rom(rom, &mut header_buf)?;

        // check for iNES format
        if header::check_ines_format_name(&header_buf) {
            // check for NES 2.0 format
            if header::check_nes_2_0_format(&header_buf) { 
                Ok(FileHeader::Nes2_0(header_buf))
            } else {
                Ok(FileHeader::INes(header_buf))
            }
        } else {
            Err(NesSystemError::UnknownFileFormat)
        }

    }

    fn i_nes_handler(rom: &mut File, header: &[u8; HEADER_SIZE], memory: &mut MainMemory) -> Result<(), NesSystemError> {

        // get the size of the PRG ROM
        let prg_rom_bytes: usize = header[4] as usize;
        let _chr_rom_bytes: usize = header[5] as usize; // if this value is 0, the board uses CHR-RAM
        
        let ines_info = header::generate_i_nes_info(header);
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
        
        let prg_rom_size = 16384 * prg_rom_bytes;

        // dynamically allocate buffer
        let mut prg_rom: Vec<u8> = vec![0; prg_rom_size];

        Self::read_from_rom(rom, &mut prg_rom[..])?;

        let mut data: [u8; MAIN_MEMORY_MAP_ADDRESSABLE_RANGE]
            = [0; MAIN_MEMORY_MAP_ADDRESSABLE_RANGE];

        let prg_start = 0x8000;
        
        for i in 0..(0x8000/prg_rom_size) { // gives the number of times the program rom needs to be mirrored
            for j in 0..prg_rom_size {

                let start_offset = (prg_start + (prg_rom_size * i)) + j;
                let write_address = match u16::try_from(start_offset) {
                    Err(_) => {return Err(NesSystemError::MemoryIOError("Address overflow when loading PRG_ROM"))},
                    Ok(x) => x
                };
                    
                data[write_address as usize] = prg_rom[j];
            }
        }

        // TODO: clean up this hack
        *memory = MainMemory::from(data);

        Ok(())
    }

    fn nes_2_0_handler(rom: &mut File, header: &[u8; HEADER_SIZE], memory: &mut MainMemory) -> Result<(), NesSystemError> {
        
        Ok(())
    }

}

pub mod nes_system_error {

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
