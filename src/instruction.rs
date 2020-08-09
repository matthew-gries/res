use crate::cpu::CPU;
use crate::memory::Memory;

use std::collections::HashMap;

#[derive(Debug)]
pub enum AddressingMode {
    ZeroPage, // the address in zero page (high byte is 00)
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
    Implied,
    Accumulator,
    Immediate, // the literal byte in memory is the operand
    Relative,
    IndexedIndirect, // Indirect, X
    IndirectIndexed // Indirect, Y
}

/// Get the operand given the addressing mode, as well as the number of extra cycles if necessary
pub fn get_operand_using_addr_mode(mode: &AddressingMode, cpu: &mut CPU, memory: &Memory) -> (u8, usize) {

    match *mode {
        AddressingMode::Immediate => {
            (cpu.read_byte_and_increment(memory), 0)
        },
        AddressingMode::ZeroPage => {
            let addr = cpu.read_byte_and_increment(memory);
            (memory.read(addr as u16), 0)
        },
        AddressingMode::ZeroPageX => {
            let addr = cpu.read_byte_and_increment(memory);
            let (addr, _) = addr.overflowing_add(cpu.x);
            (memory.read(addr as u16), 0)
        },
        AddressingMode::ZeroPageY => {
            let addr = cpu.read_byte_and_increment(memory);
            let (addr, _) = addr.overflowing_add(cpu.y);
            (memory.read(addr as u16), 0)
        },
        AddressingMode::Absolute => {
            let low_byte = cpu.read_byte_and_increment(memory);
            let high_byte = cpu.read_byte_and_increment(memory);
            let addr = ((high_byte as u16) << 8) | low_byte as u16;
            (memory.read(addr), 0)
        },
        AddressingMode::AbsoluteX => {
            let low_byte = cpu.read_byte_and_increment(memory);
            let high_byte = cpu.read_byte_and_increment(memory);
            let addr_old = ((high_byte as u16) << 8) | low_byte as u16;
            let (addr, _) = addr_old.overflowing_add(cpu.x as u16);
            let cycles = {
                if Memory::check_if_page_crossed(addr_old, addr) {
                    1
                } else {
                    0
                }
            };
            (memory.read(addr), cycles)
        },
        AddressingMode::AbsoluteY => {
            let low_byte = cpu.read_byte_and_increment(memory);
            let high_byte = cpu.read_byte_and_increment(memory);
            let addr_old = ((high_byte as u16) << 8) | low_byte as u16;
            let (addr, _) = addr_old.overflowing_add(cpu.y as u16);
            let cycles = {
                if Memory::check_if_page_crossed(addr_old, addr) {
                    1
                } else {
                    0
                }
            };
            (memory.read(addr), cycles)
        },
        AddressingMode::Indirect => {
            let low_byte = cpu.read_byte_and_increment(memory);
            let high_byte = cpu.read_byte_and_increment(memory);
            let addr = ((high_byte as u16) << 8) | low_byte as u16;
            (memory.read(addr), 0)
        },
        AddressingMode::Implied | AddressingMode::Accumulator => (0, 0),
        AddressingMode::Relative => {
            (cpu.read_byte_and_increment(memory) , 0)
        },
        AddressingMode::IndexedIndirect => {
            let addr = cpu.read_byte_and_increment(memory);
            let (low_byte_addr, _) = addr.overflowing_add(cpu.x);
            let (high_byte_addr, _) = low_byte_addr.overflowing_add(1);
            let low_byte = memory.read(low_byte_addr as u16);
            let high_byte = memory.read(high_byte_addr as u16);
            let addr = ((high_byte as u16) << 8) | low_byte as u16;
            (memory.read(addr), 0)
        },
        AddressingMode::IndirectIndexed => {
            let low_byte_addr = cpu.read_byte_and_increment(memory);
            let (high_byte_addr, _) = low_byte_addr.overflowing_add(1);
            let low_byte = memory.read(low_byte_addr as u16);
            let high_byte = memory.read(high_byte_addr as u16);
            let addr_old = ((high_byte as u16) << 8) | low_byte as u16;
            let (addr, _) = addr_old.overflowing_add(cpu.y as u16);
            let cycles = {
                if Memory::check_if_page_crossed(addr_old, addr) {
                    1
                } else {
                    0
                }
            };
            (memory.read(addr), cycles)
        }
    }
}

lazy_static! {
    pub static ref INSTRUCTION_TABLE: HashMap<u8, Instruction> = {
        let mut map = HashMap::new();
        // ADC
        map.insert(0x69, Instruction::ADC(AddressingMode::Immediate, 2, 2));
        map.insert(0x65, Instruction::ADC(AddressingMode::ZeroPage, 2, 3));
        map.insert(0x75, Instruction::ADC(AddressingMode::ZeroPageX, 2, 4));
        map.insert(0x6D, Instruction::ADC(AddressingMode::Absolute, 3, 4));
        map.insert(0x7D, Instruction::ADC(AddressingMode::AbsoluteX, 3, 4));
        map.insert(0x79, Instruction::ADC(AddressingMode::AbsoluteY, 3, 4));
        map.insert(0x61, Instruction::ADC(AddressingMode::IndexedIndirect, 2, 6));
        map.insert(0x71, Instruction::ADC(AddressingMode::IndirectIndexed, 2, 5));
        // AND
        map.insert(0x29, Instruction::AND(AddressingMode::Immediate, 2, 2));
        map.insert(0x25, Instruction::AND(AddressingMode::ZeroPage, 2, 3));
        map.insert(0x35, Instruction::AND(AddressingMode::ZeroPageX, 2, 4));
        map.insert(0x2D, Instruction::AND(AddressingMode::Absolute, 3, 4));
        map.insert(0x3D, Instruction::AND(AddressingMode::AbsoluteX, 3, 4));
        map.insert(0x21, Instruction::AND(AddressingMode::IndexedIndirect, 2, 6));
        map.insert(0x31, Instruction::AND(AddressingMode::IndirectIndexed, 2, 5));
        // ASL
        map.insert(0x0A, Instruction::ASL(AddressingMode::Accumulator, 1, 2));
        map.insert(0x06, Instruction::ASL(AddressingMode::ZeroPage, 2, 5));
        map.insert(0x16, Instruction::ASL(AddressingMode::ZeroPageX, 2, 6));
        map.insert(0x0E, Instruction::ASL(AddressingMode::Absolute, 3, 6));
        map.insert(0x1E, Instruction::ASL(AddressingMode::AbsoluteX, 3, 7));
        // BIT
        map.insert(0x24, Instruction::BIT(AddressingMode::ZeroPage, 2, 3));
        map.insert(0x2C, Instruction::BIT(AddressingMode::Absolute, 3, 4));
        // Branch instructions
        map.insert(0x10, Instruction::BPL(AddressingMode::Relative, 2, 2));
        map.insert(0x30, Instruction::BMI(AddressingMode::Relative, 2, 2));
        map.insert(0x50, Instruction::BVC(AddressingMode::Relative, 2, 2));
        map.insert(0x70, Instruction::BVS(AddressingMode::Relative, 2, 2));
        map.insert(0x90, Instruction::BCC(AddressingMode::Relative, 2, 2));
        map.insert(0xB0, Instruction::BCS(AddressingMode::Relative, 2, 2));
        map.insert(0xD0, Instruction::BNE(AddressingMode::Relative, 2, 2));
        map.insert(0xF0, Instruction::BEQ(AddressingMode::Relative, 2, 2));
        // BRK
        map.insert(0x00, Instruction::BRK(AddressingMode::Implied, 1, 7));
        // CMP
        map.insert(0xC9, Instruction::CMP(AddressingMode::Immediate, 2, 2));
        map.insert(0xC5, Instruction::CMP(AddressingMode::ZeroPage, 2, 3));
        map.insert(0xD5, Instruction::CMP(AddressingMode::ZeroPageX, 2, 4));
        map.insert(0xCD, Instruction::CMP(AddressingMode::Absolute, 3, 4));
        map.insert(0xDD, Instruction::CMP(AddressingMode::AbsoluteX, 3, 4));
        map.insert(0xD9, Instruction::CMP(AddressingMode::AbsoluteY, 3, 4));
        map.insert(0xC1, Instruction::CMP(AddressingMode::IndexedIndirect, 2, 6));
        map.insert(0xD1, Instruction::CMP(AddressingMode::IndirectIndexed, 2, 5));
        // CPX
        map.insert(0xE0, Instruction::CPX(AddressingMode::Immediate, 2, 2));
        map.insert(0xE4, Instruction::CPX(AddressingMode::ZeroPage, 2, 3));
        map.insert(0xEC, Instruction::CPX(AddressingMode::Absolute, 3, 4));
        // CPY
        map.insert(0xC0, Instruction::CPY(AddressingMode::Immediate, 2, 2));
        map.insert(0xC4, Instruction::CPY(AddressingMode::ZeroPage, 2, 3));
        map.insert(0xCC, Instruction::CPY(AddressingMode::Absolute, 3, 4));
        // DEC
        map.insert(0xC6, Instruction::DEC(AddressingMode::ZeroPage, 2, 5));
        map.insert(0xD6, Instruction::DEC(AddressingMode::ZeroPageX, 2, 6));
        map.insert(0xCE, Instruction::DEC(AddressingMode::Absolute, 3, 6));
        map.insert(0xDE, Instruction::DEC(AddressingMode::AbsoluteX, 3, 7));
        // EOR
        map.insert(0x49, Instruction::EOR(AddressingMode::Immediate, 2, 2));
        map.insert(0x45, Instruction::EOR(AddressingMode::ZeroPage, 2, 3));
        map.insert(0x55, Instruction::EOR(AddressingMode::ZeroPageX, 2, 4));
        map.insert(0x4D, Instruction::EOR(AddressingMode::Absolute, 3, 4));
        map.insert(0x5D, Instruction::EOR(AddressingMode::AbsoluteX, 3, 4));
        map.insert(0x59, Instruction::EOR(AddressingMode::AbsoluteY, 3, 4));
        map.insert(0x41, Instruction::EOR(AddressingMode::IndexedIndirect, 2, 6));
        map.insert(0x51, Instruction::EOR(AddressingMode::IndirectIndexed, 2, 5));
        // Flag settings
        map.insert(0x18, Instruction::CLC(AddressingMode::Implied, 1, 2));
        map.insert(0x38, Instruction::SEC(AddressingMode::Implied, 1, 2));
        map.insert(0x58, Instruction::CLI(AddressingMode::Implied, 1, 2));
        map.insert(0x78, Instruction::SEI(AddressingMode::Implied, 1, 2));
        map.insert(0xB8, Instruction::CLV(AddressingMode::Implied, 1, 2));
        map.insert(0xD8, Instruction::CLD(AddressingMode::Implied, 1, 2));
        map.insert(0xF8, Instruction::SED(AddressingMode::Implied, 1, 2));
        // INC
        map.insert(0xE6, Instruction::INC(AddressingMode::ZeroPage, 2, 5));
        map.insert(0xF6, Instruction::INC(AddressingMode::ZeroPageX, 2, 6));
        map.insert(0xEE, Instruction::INC(AddressingMode::Absolute, 3, 6));
        map.insert(0xFE, Instruction::INC(AddressingMode::AbsoluteX, 3, 7));
        // JMP
        map.insert(0x4C, Instruction::JMP(AddressingMode::Absolute, 3, 3));
        map.insert(0x6C, Instruction::JMP(AddressingMode::Indirect, 3, 5));
        // JSR
        map.insert(0x20, Instruction::JSR(AddressingMode::Absolute, 3, 6));
        // LDA
        map.insert(0xA9, Instruction::LDA(AddressingMode::Immediate, 2, 2));
        map.insert(0xA5, Instruction::LDA(AddressingMode::ZeroPage, 2, 3));
        map.insert(0xB5, Instruction::LDA(AddressingMode::ZeroPageX, 2, 4));
        map.insert(0xAD, Instruction::LDA(AddressingMode::Absolute, 3, 4));
        map.insert(0xBD, Instruction::LDA(AddressingMode::AbsoluteX, 3, 4));
        map.insert(0xB9, Instruction::LDA(AddressingMode::AbsoluteY, 3, 4));
        map.insert(0xA1, Instruction::LDA(AddressingMode::IndexedIndirect, 2, 6));
        map.insert(0xB1, Instruction::LDA(AddressingMode::IndirectIndexed, 2, 5));
        // LDX
        map.insert(0xA2, Instruction::LDX(AddressingMode::Immediate, 2, 2));
        map.insert(0xA6, Instruction::LDX(AddressingMode::ZeroPage, 2, 3));
        map.insert(0xB6, Instruction::LDX(AddressingMode::ZeroPageY, 2, 4));
        map.insert(0xAE, Instruction::LDX(AddressingMode::Absolute, 3, 4));
        map.insert(0xBE, Instruction::LDX(AddressingMode::AbsoluteY, 3, 4));
        // LDY
        map.insert(0xA0, Instruction::LDY(AddressingMode::Immediate, 2, 2));
        map.insert(0xA4, Instruction::LDY(AddressingMode::ZeroPage, 2, 3));
        map.insert(0xB4, Instruction::LDY(AddressingMode::ZeroPageX, 2, 4));
        map.insert(0xAC, Instruction::LDY(AddressingMode::Absolute, 3, 4));
        map.insert(0xBC, Instruction::LDY(AddressingMode::AbsoluteX, 3, 4));
        // LSR
        map.insert(0x4A, Instruction::LSR(AddressingMode::Accumulator, 1, 2));
        map.insert(0x46, Instruction::LSR(AddressingMode::ZeroPage, 2, 5));
        map.insert(0x56, Instruction::LSR(AddressingMode::ZeroPageX, 2, 6));
        map.insert(0x4E, Instruction::LSR(AddressingMode::Absolute, 3, 6));
        map.insert(0x5E, Instruction::LSR(AddressingMode::AbsoluteX, 3, 7));
        // NOP
        map.insert(0xEA, Instruction::NOP(AddressingMode::Implied, 1, 2));
        // ORA
        map.insert(0x09, Instruction::ORA(AddressingMode::Immediate, 2, 2));
        map.insert(0x05, Instruction::ORA(AddressingMode::ZeroPage, 2, 3));
        map.insert(0x15, Instruction::ORA(AddressingMode::ZeroPageX, 2, 4));
        map.insert(0x0D, Instruction::ORA(AddressingMode::Absolute, 3, 4));
        map.insert(0x1D, Instruction::ORA(AddressingMode::AbsoluteX, 3, 4));
        map.insert(0x19, Instruction::ORA(AddressingMode::AbsoluteY, 3, 4));
        map.insert(0x01, Instruction::ORA(AddressingMode::IndexedIndirect, 2, 6));
        map.insert(0x11, Instruction::ORA(AddressingMode::IndirectIndexed, 2, 5));
        // Register instructions
        map.insert(0xAA, Instruction::TAX(AddressingMode::Implied, 1, 2));
        map.insert(0x8A, Instruction::TXA(AddressingMode::Implied, 1, 2));
        map.insert(0xCA, Instruction::DEX(AddressingMode::Implied, 1, 2));
        map.insert(0xEA, Instruction::INX(AddressingMode::Implied, 1, 2));
        map.insert(0xA8, Instruction::TAY(AddressingMode::Implied, 1, 2));
        map.insert(0x98, Instruction::TYA(AddressingMode::Implied, 1, 2));
        map.insert(0x88, Instruction::DEY(AddressingMode::Implied, 1, 2));
        map.insert(0xC8, Instruction::INY(AddressingMode::Implied, 1, 2));
        // ROL
        map.insert(0x2A, Instruction::ROL(AddressingMode::Accumulator, 1, 2));
        map.insert(0x26, Instruction::ROL(AddressingMode::ZeroPage, 2, 5));
        map.insert(0x36, Instruction::ROL(AddressingMode::ZeroPageX, 2, 6));
        map.insert(0x2E, Instruction::ROL(AddressingMode::Absolute, 3, 6));
        map.insert(0x3E, Instruction::ROL(AddressingMode::AbsoluteX, 3, 7));
        // ROR
        map.insert(0x6A, Instruction::ROR(AddressingMode::Accumulator, 1, 2));
        map.insert(0x66, Instruction::ROR(AddressingMode::ZeroPage, 2, 5));
        map.insert(0x76, Instruction::ROR(AddressingMode::ZeroPageX, 2, 6));
        map.insert(0x6E, Instruction::ROR(AddressingMode::Absolute, 3, 6));
        map.insert(0x7E, Instruction::ROR(AddressingMode::AbsoluteX, 3, 7));
        // RTI
        map.insert(0x40, Instruction::RTI(AddressingMode::Implied, 1, 6));
        // RTS
        map.insert(0x60, Instruction::RTS(AddressingMode::Implied, 1, 6));
        // SBC
        map.insert(0xE9, Instruction::SBC(AddressingMode::Immediate, 2, 2));
        map.insert(0xE5, Instruction::SBC(AddressingMode::ZeroPage, 2, 3));
        map.insert(0xF5, Instruction::SBC(AddressingMode::ZeroPageX, 2, 4));
        map.insert(0xED, Instruction::SBC(AddressingMode::Absolute, 3, 4));
        map.insert(0xFD, Instruction::SBC(AddressingMode::AbsoluteX, 3, 4));
        map.insert(0xF9, Instruction::SBC(AddressingMode::AbsoluteY, 3, 4));
        map.insert(0xE1, Instruction::SBC(AddressingMode::IndexedIndirect, 2, 6));
        map.insert(0xF1, Instruction::SBC(AddressingMode::IndirectIndexed, 2, 5));
        // STA
        map.insert(0x85, Instruction::STA(AddressingMode::ZeroPage, 2, 3));
        map.insert(0x95, Instruction::STA(AddressingMode::ZeroPageX, 2, 4));
        map.insert(0x8D, Instruction::STA(AddressingMode::Absolute, 3, 4));
        map.insert(0x9D, Instruction::STA(AddressingMode::AbsoluteX, 3, 5));
        map.insert(0x99, Instruction::STA(AddressingMode::AbsoluteY, 3, 5));
        map.insert(0x81, Instruction::STA(AddressingMode::IndexedIndirect, 2, 6));
        map.insert(0x91, Instruction::STA(AddressingMode::IndirectIndexed, 2, 6));
        // Stack instructions
        map.insert(0x9A, Instruction::TXS(AddressingMode::Implied, 1, 2));
        map.insert(0xBA, Instruction::TSX(AddressingMode::Implied, 1, 2));
        map.insert(0x48, Instruction::PHA(AddressingMode::Implied, 1, 3));
        map.insert(0x68, Instruction::PLA(AddressingMode::Implied, 1, 4));
        map.insert(0x08, Instruction::PHP(AddressingMode::Implied, 1, 3));
        map.insert(0x28, Instruction::PLP(AddressingMode::Implied, 1, 4));
        // STX
        map.insert(0x86, Instruction::STX(AddressingMode::ZeroPage, 2, 3));
        map.insert(0x96, Instruction::STX(AddressingMode::ZeroPageY, 2, 4));
        map.insert(0x8E, Instruction::STX(AddressingMode::Absolute, 3, 4));
        // STY
        map.insert(0x84, Instruction::STY(AddressingMode::ZeroPage, 2, 3));
        map.insert(0x94, Instruction::STY(AddressingMode::ZeroPageX, 2, 4));
        map.insert(0x8C, Instruction::STY(AddressingMode::Absolute, 3, 4));

        map
    };
}

#[derive(Debug)]
pub enum Instruction {
    ADC(AddressingMode, usize, usize),
    AND(AddressingMode, usize, usize),
    ASL(AddressingMode, usize, usize),
    BIT(AddressingMode, usize, usize),
    BPL(AddressingMode, usize, usize),
    BMI(AddressingMode, usize, usize),
    BVC(AddressingMode, usize, usize),
    BVS(AddressingMode, usize, usize),
    BCC(AddressingMode, usize, usize),
    BCS(AddressingMode, usize, usize),
    BNE(AddressingMode, usize, usize),
    BEQ(AddressingMode, usize, usize),
    BRK(AddressingMode, usize, usize),
    CMP(AddressingMode, usize, usize),
    CPX(AddressingMode, usize, usize),
    CPY(AddressingMode, usize, usize),
    DEC(AddressingMode, usize, usize),
    EOR(AddressingMode, usize, usize),
    CLC(AddressingMode, usize, usize),
    SEC(AddressingMode, usize, usize),
    CLI(AddressingMode, usize, usize),
    SEI(AddressingMode, usize, usize),
    CLV(AddressingMode, usize, usize),
    CLD(AddressingMode, usize, usize),
    SED(AddressingMode, usize, usize),
    INC(AddressingMode, usize, usize),
    JMP(AddressingMode, usize, usize),
    JSR(AddressingMode, usize, usize),
    LDA(AddressingMode, usize, usize),
    LDX(AddressingMode, usize, usize),
    LDY(AddressingMode, usize, usize),
    LSR(AddressingMode, usize, usize),
    NOP(AddressingMode, usize, usize),
    ORA(AddressingMode, usize, usize),
    TAX(AddressingMode, usize, usize),
    TXA(AddressingMode, usize, usize),
    DEX(AddressingMode, usize, usize),
    INX(AddressingMode, usize, usize),
    TAY(AddressingMode, usize, usize),
    TYA(AddressingMode, usize, usize),
    DEY(AddressingMode, usize, usize),
    INY(AddressingMode, usize, usize),
    ROL(AddressingMode, usize, usize),
    ROR(AddressingMode, usize, usize),
    RTI(AddressingMode, usize, usize),
    RTS(AddressingMode, usize, usize),
    SBC(AddressingMode, usize, usize),
    STA(AddressingMode, usize, usize),
    TXS(AddressingMode, usize, usize),
    TSX(AddressingMode, usize, usize),
    PHA(AddressingMode, usize, usize),
    PLA(AddressingMode, usize, usize),
    PHP(AddressingMode, usize, usize),
    PLP(AddressingMode, usize, usize),
    STX(AddressingMode, usize, usize),
    STY(AddressingMode, usize, usize),
}

pub mod instruction_func {

    use super::*;

    /// Run an ADC instruction and return the number of cycles it took to complete the instruction
    pub fn adc(cpu: &mut CPU, memory: &Memory, mode: &AddressingMode, len: usize, time: usize) -> usize {

        let (operand, extra_cycles) = match *mode {
            AddressingMode::Immediate | AddressingMode::ZeroPage | AddressingMode::ZeroPageX
            | AddressingMode::Absolute | AddressingMode::AbsoluteX | AddressingMode::AbsoluteY
            | AddressingMode::IndexedIndirect | AddressingMode::IndirectIndexed =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for ADC", *mode),
        };

        let (result_temp, carry1) = cpu.a.overflowing_add(operand);
        let (result, carry2) = result_temp.overflowing_add(cpu.p.c as u8);
        let carry_bit = carry1 || carry2;

        cpu.p.z = result == 0;
        cpu.p.c = carry_bit;
        cpu.p.n = CPU::check_if_neg(result);
        // using overflowing_add on an unsigned 8-bit checks for overflow in bit 7, and as such works as the carry
        // bit. We need to use the value of A and the operand and compare to the result to see if there is signed
        // overflow (an invalid two's complement result)
        let (_, signed_overflow) = (cpu.a as i8).overflowing_add(operand as i8); // TODO: is this the best way to do this?
        cpu.p.v = signed_overflow;

        cpu.a = result;
        let cycles = time + extra_cycles;
        cycles
    }

    pub fn lda(cpu: &mut CPU, memory: &Memory, mode: &AddressingMode, len: usize, time: usize) -> usize {

        let (operand, extra_cycles) = match *mode {
            AddressingMode::Immediate | AddressingMode::ZeroPage | AddressingMode::ZeroPageX
            | AddressingMode::Absolute | AddressingMode::AbsoluteX | AddressingMode::AbsoluteY
            | AddressingMode::IndexedIndirect | AddressingMode::IndirectIndexed =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for LDA", *mode),
        };

        cpu.a = operand;

        cpu.p.n =  CPU::check_if_neg(operand);
        cpu.p.z = operand == 0;

        let cycles = time + extra_cycles;
        cycles
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn generate_cpu_and_mem() -> (CPU, Memory) {
        (CPU::new(), Memory::new())
    }

    #[test]
    fn lda_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xA9);
        mem.write(1, 0x44);
        let instr = INSTRUCTION_TABLE.get(&0xA9).unwrap();
        if let Instruction::LDA(mode, len, time) = instr {
            cpu.read_byte_and_increment(memory); 
            let cycles = instruction_func::lda(&mut cpu, &mem, mode, *len, *time);
            assert_eq!(cpu.a, 0x44);
	    assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        }
    }

}
