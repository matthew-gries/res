use crate::cpu::CPU;
use crate::memory::Memory;

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
