use crate::cpu::CPU;
use crate::memory::Memory;

use std::collections::HashMap;

pub enum AddressingMode {
    ZeroPage,
    IndexedZeroPage,
    Absolute,
    IndexedAbsolute,
    Indirect,
    Implied,
    Accumulator,
    Immediate,
    Relative,
    IndexedIndirect,
    IndirectIndexed
}

lazy_static! {
    pub static ref INSTRUCTION_TABLE: HashMap<u8, Instruction> = {
        let mut map = HashMap::new();
        map.insert(0x69, Instruction::ADC(AddressingMode::Immediate, 2, 2));
        map
    };
}

pub enum Instruction {
    ADC(AddressingMode, usize, usize),
}

pub mod instruction_func {

    use super::*;

    pub fn adc(cpu: &mut CPU, memory: &Memory, mode: &AddressingMode, len: usize, time: usize) {

        let operand = match *mode {
            AddressingMode::Immediate => {
                cpu.get_byte_and_increment(memory)
            }
            _ => 0
        };
        

        let (result_temp, carry1) = cpu.a.overflowing_add(operand);
        let (result, carry2) = result_temp.overflowing_add(cpu.p.c as u8);
        let carry_bit = carry1 || carry2;

        cpu.a = result;
        cpu.p.c = carry_bit;
    }
}

