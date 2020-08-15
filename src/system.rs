use crate::cpu::CPU;
use crate::mapper;
use crate::mapper::Mapper;
use crate::memory::Memory;
use crate::memory::MEMORY_MAP_ADDRESSABLE_RANGE;

use nes_system_error::NesSystemError;

use std::fs::File;
use std::io::prelude::*;

const HEADER_SIZE: usize = 16;

pub struct NesSystem {
    cpu: CPU,
    memory: Memory
}

impl NesSystem {

    pub fn new(cpu: CPU, memory: Memory) -> Self {
        NesSystem{cpu, memory}
    }

    pub fn start(&mut self) {
        loop {
            self.cpu.instruction_cycle(&mut self.memory).unwrap();
        }
    }
}
