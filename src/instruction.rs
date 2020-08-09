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
pub fn get_operand_using_addr_mode(mode: &AddressingMode, cpu: &mut CPU, memory: &Memory) -> (u16, usize) {

    match *mode {
        AddressingMode::Immediate => {
            (cpu.read_byte_and_increment(memory) as u16, 0)
        },
        AddressingMode::ZeroPage => {
            let addr = cpu.read_byte_and_increment(memory);
            (addr as u16, 0)
        },
        AddressingMode::ZeroPageX => {
            let addr = cpu.read_byte_and_increment(memory);
            let (addr, _) = addr.overflowing_add(cpu.x);
            (addr as u16, 0)
        },
        AddressingMode::ZeroPageY => {
            let addr = cpu.read_byte_and_increment(memory);
            let (addr, _) = addr.overflowing_add(cpu.y);
            (addr as u16, 0)
        },
        AddressingMode::Absolute => {
            let low_byte = cpu.read_byte_and_increment(memory);
            let high_byte = cpu.read_byte_and_increment(memory);
            let addr = ((high_byte as u16) << 8) | low_byte as u16;
            (addr, 0)
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
            (addr, cycles)
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
            (addr, cycles)
        },
        AddressingMode::Indirect => {
            let low_byte = cpu.read_byte_and_increment(memory);
            let high_byte = cpu.read_byte_and_increment(memory);
            let addr = ((high_byte as u16) << 8) | low_byte as u16;
            (addr, 0)
        },
        AddressingMode::Implied | AddressingMode::Accumulator => (0, 0),
        AddressingMode::Relative => {
            (cpu.read_byte_and_increment(memory) as u16 , 0)
        },
        AddressingMode::IndexedIndirect => {
            let addr = cpu.read_byte_and_increment(memory);
            let (low_byte_addr, _) = addr.overflowing_add(cpu.x);
            let (high_byte_addr, _) = low_byte_addr.overflowing_add(1);
            let low_byte = memory.read(low_byte_addr as u16);
            let high_byte = memory.read(high_byte_addr as u16);
            let addr = ((high_byte as u16) << 8) | low_byte as u16;
            (addr, 0)
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
            (addr, cycles)
        }
    }
}

lazy_static! {
    pub static ref INSTRUCTION_TABLE: HashMap<u8, Instruction> = {
        let mut map = HashMap::new();
        // ADC
        map.insert(0x69, Instruction::ADC(AddressingMode::Immediate, 2));
        map.insert(0x65, Instruction::ADC(AddressingMode::ZeroPage, 3));
        map.insert(0x75, Instruction::ADC(AddressingMode::ZeroPageX, 4));
        map.insert(0x6D, Instruction::ADC(AddressingMode::Absolute, 4));
        map.insert(0x7D, Instruction::ADC(AddressingMode::AbsoluteX, 4));
        map.insert(0x79, Instruction::ADC(AddressingMode::AbsoluteY, 4));
        map.insert(0x61, Instruction::ADC(AddressingMode::IndexedIndirect, 6));
        map.insert(0x71, Instruction::ADC(AddressingMode::IndirectIndexed, 5));
        // AND
        map.insert(0x29, Instruction::AND(AddressingMode::Immediate, 2));
        map.insert(0x25, Instruction::AND(AddressingMode::ZeroPage, 3));
        map.insert(0x35, Instruction::AND(AddressingMode::ZeroPageX, 4));
        map.insert(0x2D, Instruction::AND(AddressingMode::Absolute, 4));
        map.insert(0x3D, Instruction::AND(AddressingMode::AbsoluteX, 4));
        map.insert(0x21, Instruction::AND(AddressingMode::IndexedIndirect, 6));
        map.insert(0x31, Instruction::AND(AddressingMode::IndirectIndexed, 5));
        // ASL
        map.insert(0x0A, Instruction::ASL(AddressingMode::Accumulator, 2));
        map.insert(0x06, Instruction::ASL(AddressingMode::ZeroPage, 5));
        map.insert(0x16, Instruction::ASL(AddressingMode::ZeroPageX, 6));
        map.insert(0x0E, Instruction::ASL(AddressingMode::Absolute, 6));
        map.insert(0x1E, Instruction::ASL(AddressingMode::AbsoluteX, 7));
        // BIT
        map.insert(0x24, Instruction::BIT(AddressingMode::ZeroPage, 3));
        map.insert(0x2C, Instruction::BIT(AddressingMode::Absolute, 4));
        // Branch instructions
        map.insert(0x10, Instruction::BPL(AddressingMode::Relative, 2));
        map.insert(0x30, Instruction::BMI(AddressingMode::Relative, 2));
        map.insert(0x50, Instruction::BVC(AddressingMode::Relative, 2));
        map.insert(0x70, Instruction::BVS(AddressingMode::Relative, 2));
        map.insert(0x90, Instruction::BCC(AddressingMode::Relative, 2));
        map.insert(0xB0, Instruction::BCS(AddressingMode::Relative, 2));
        map.insert(0xD0, Instruction::BNE(AddressingMode::Relative, 2));
        map.insert(0xF0, Instruction::BEQ(AddressingMode::Relative, 2));
        // BRK
        map.insert(0x00, Instruction::BRK(AddressingMode::Implied, 7));
        // CMP
        map.insert(0xC9, Instruction::CMP(AddressingMode::Immediate, 2));
        map.insert(0xC5, Instruction::CMP(AddressingMode::ZeroPage, 3));
        map.insert(0xD5, Instruction::CMP(AddressingMode::ZeroPageX, 4));
        map.insert(0xCD, Instruction::CMP(AddressingMode::Absolute, 4));
        map.insert(0xDD, Instruction::CMP(AddressingMode::AbsoluteX, 4));
        map.insert(0xD9, Instruction::CMP(AddressingMode::AbsoluteY, 4));
        map.insert(0xC1, Instruction::CMP(AddressingMode::IndexedIndirect, 6));
        map.insert(0xD1, Instruction::CMP(AddressingMode::IndirectIndexed, 5));
        // CPX
        map.insert(0xE0, Instruction::CPX(AddressingMode::Immediate, 2));
        map.insert(0xE4, Instruction::CPX(AddressingMode::ZeroPage, 3));
        map.insert(0xEC, Instruction::CPX(AddressingMode::Absolute, 4));
        // CPY
        map.insert(0xC0, Instruction::CPY(AddressingMode::Immediate, 2));
        map.insert(0xC4, Instruction::CPY(AddressingMode::ZeroPage, 3));
        map.insert(0xCC, Instruction::CPY(AddressingMode::Absolute, 4));
        // DEC
        map.insert(0xC6, Instruction::DEC(AddressingMode::ZeroPage, 5));
        map.insert(0xD6, Instruction::DEC(AddressingMode::ZeroPageX, 6));
        map.insert(0xCE, Instruction::DEC(AddressingMode::Absolute, 6));
        map.insert(0xDE, Instruction::DEC(AddressingMode::AbsoluteX, 7));
        // EOR
        map.insert(0x49, Instruction::EOR(AddressingMode::Immediate, 2));
        map.insert(0x45, Instruction::EOR(AddressingMode::ZeroPage, 3));
        map.insert(0x55, Instruction::EOR(AddressingMode::ZeroPageX, 4));
        map.insert(0x4D, Instruction::EOR(AddressingMode::Absolute, 4));
        map.insert(0x5D, Instruction::EOR(AddressingMode::AbsoluteX, 4));
        map.insert(0x59, Instruction::EOR(AddressingMode::AbsoluteY, 4));
        map.insert(0x41, Instruction::EOR(AddressingMode::IndexedIndirect, 6));
        map.insert(0x51, Instruction::EOR(AddressingMode::IndirectIndexed, 5));
        // Flag settings
        map.insert(0x18, Instruction::CLC(AddressingMode::Implied, 2));
        map.insert(0x38, Instruction::SEC(AddressingMode::Implied, 2));
        map.insert(0x58, Instruction::CLI(AddressingMode::Implied, 2));
        map.insert(0x78, Instruction::SEI(AddressingMode::Implied, 2));
        map.insert(0xB8, Instruction::CLV(AddressingMode::Implied, 2));
        map.insert(0xD8, Instruction::CLD(AddressingMode::Implied, 2));
        map.insert(0xF8, Instruction::SED(AddressingMode::Implied, 2));
        // INC
        map.insert(0xE6, Instruction::INC(AddressingMode::ZeroPage, 5));
        map.insert(0xF6, Instruction::INC(AddressingMode::ZeroPageX, 6));
        map.insert(0xEE, Instruction::INC(AddressingMode::Absolute, 6));
        map.insert(0xFE, Instruction::INC(AddressingMode::AbsoluteX, 7));
        // JMP
        map.insert(0x4C, Instruction::JMP(AddressingMode::Absolute, 3));
        map.insert(0x6C, Instruction::JMP(AddressingMode::Indirect, 5));
        // JSR
        map.insert(0x20, Instruction::JSR(AddressingMode::Absolute, 6));
        // LDA
        map.insert(0xA9, Instruction::LDA(AddressingMode::Immediate, 2));
        map.insert(0xA5, Instruction::LDA(AddressingMode::ZeroPage, 3));
        map.insert(0xB5, Instruction::LDA(AddressingMode::ZeroPageX, 4));
        map.insert(0xAD, Instruction::LDA(AddressingMode::Absolute, 4));
        map.insert(0xBD, Instruction::LDA(AddressingMode::AbsoluteX, 4));
        map.insert(0xB9, Instruction::LDA(AddressingMode::AbsoluteY, 4));
        map.insert(0xA1, Instruction::LDA(AddressingMode::IndexedIndirect, 6));
        map.insert(0xB1, Instruction::LDA(AddressingMode::IndirectIndexed, 5));
        // LDX
        map.insert(0xA2, Instruction::LDX(AddressingMode::Immediate, 2));
        map.insert(0xA6, Instruction::LDX(AddressingMode::ZeroPage, 3));
        map.insert(0xB6, Instruction::LDX(AddressingMode::ZeroPageY, 4));
        map.insert(0xAE, Instruction::LDX(AddressingMode::Absolute, 4));
        map.insert(0xBE, Instruction::LDX(AddressingMode::AbsoluteY, 4));
        // LDY
        map.insert(0xA0, Instruction::LDY(AddressingMode::Immediate, 2));
        map.insert(0xA4, Instruction::LDY(AddressingMode::ZeroPage, 3));
        map.insert(0xB4, Instruction::LDY(AddressingMode::ZeroPageX, 4));
        map.insert(0xAC, Instruction::LDY(AddressingMode::Absolute, 4));
        map.insert(0xBC, Instruction::LDY(AddressingMode::AbsoluteX, 4));
        // LSR
        map.insert(0x4A, Instruction::LSR(AddressingMode::Accumulator, 2));
        map.insert(0x46, Instruction::LSR(AddressingMode::ZeroPage, 5));
        map.insert(0x56, Instruction::LSR(AddressingMode::ZeroPageX, 6));
        map.insert(0x4E, Instruction::LSR(AddressingMode::Absolute, 6));
        map.insert(0x5E, Instruction::LSR(AddressingMode::AbsoluteX, 7));
        // NOP
        map.insert(0xEA, Instruction::NOP(AddressingMode::Implied, 2));
        // ORA
        map.insert(0x09, Instruction::ORA(AddressingMode::Immediate, 2));
        map.insert(0x05, Instruction::ORA(AddressingMode::ZeroPage, 3));
        map.insert(0x15, Instruction::ORA(AddressingMode::ZeroPageX, 4));
        map.insert(0x0D, Instruction::ORA(AddressingMode::Absolute, 4));
        map.insert(0x1D, Instruction::ORA(AddressingMode::AbsoluteX, 4));
        map.insert(0x19, Instruction::ORA(AddressingMode::AbsoluteY, 4));
        map.insert(0x01, Instruction::ORA(AddressingMode::IndexedIndirect, 6));
        map.insert(0x11, Instruction::ORA(AddressingMode::IndirectIndexed, 5));
        // Register instructions
        map.insert(0xAA, Instruction::TAX(AddressingMode::Implied, 2));
        map.insert(0x8A, Instruction::TXA(AddressingMode::Implied, 2));
        map.insert(0xCA, Instruction::DEX(AddressingMode::Implied, 2));
        map.insert(0xEA, Instruction::INX(AddressingMode::Implied, 2));
        map.insert(0xA8, Instruction::TAY(AddressingMode::Implied, 2));
        map.insert(0x98, Instruction::TYA(AddressingMode::Implied, 2));
        map.insert(0x88, Instruction::DEY(AddressingMode::Implied, 2));
        map.insert(0xC8, Instruction::INY(AddressingMode::Implied, 2));
        // ROL
        map.insert(0x2A, Instruction::ROL(AddressingMode::Accumulator, 2));
        map.insert(0x26, Instruction::ROL(AddressingMode::ZeroPage, 5));
        map.insert(0x36, Instruction::ROL(AddressingMode::ZeroPageX, 6));
        map.insert(0x2E, Instruction::ROL(AddressingMode::Absolute, 6));
        map.insert(0x3E, Instruction::ROL(AddressingMode::AbsoluteX, 7));
        // ROR
        map.insert(0x6A, Instruction::ROR(AddressingMode::Accumulator, 2));
        map.insert(0x66, Instruction::ROR(AddressingMode::ZeroPage, 5));
        map.insert(0x76, Instruction::ROR(AddressingMode::ZeroPageX, 6));
        map.insert(0x6E, Instruction::ROR(AddressingMode::Absolute, 6));
        map.insert(0x7E, Instruction::ROR(AddressingMode::AbsoluteX, 7));
        // RTI
        map.insert(0x40, Instruction::RTI(AddressingMode::Implied, 6));
        // RTS
        map.insert(0x60, Instruction::RTS(AddressingMode::Implied, 6));
        // SBC
        map.insert(0xE9, Instruction::SBC(AddressingMode::Immediate, 2));
        map.insert(0xE5, Instruction::SBC(AddressingMode::ZeroPage, 3));
        map.insert(0xF5, Instruction::SBC(AddressingMode::ZeroPageX, 4));
        map.insert(0xED, Instruction::SBC(AddressingMode::Absolute, 4));
        map.insert(0xFD, Instruction::SBC(AddressingMode::AbsoluteX, 4));
        map.insert(0xF9, Instruction::SBC(AddressingMode::AbsoluteY, 4));
        map.insert(0xE1, Instruction::SBC(AddressingMode::IndexedIndirect, 6));
        map.insert(0xF1, Instruction::SBC(AddressingMode::IndirectIndexed, 5));
        // STA
        map.insert(0x85, Instruction::STA(AddressingMode::ZeroPage, 3));
        map.insert(0x95, Instruction::STA(AddressingMode::ZeroPageX, 4));
        map.insert(0x8D, Instruction::STA(AddressingMode::Absolute, 4));
        map.insert(0x9D, Instruction::STA(AddressingMode::AbsoluteX, 5));
        map.insert(0x99, Instruction::STA(AddressingMode::AbsoluteY, 5));
        map.insert(0x81, Instruction::STA(AddressingMode::IndexedIndirect, 6));
        map.insert(0x91, Instruction::STA(AddressingMode::IndirectIndexed, 6));
        // Stack instructions
        map.insert(0x9A, Instruction::TXS(AddressingMode::Implied, 2));
        map.insert(0xBA, Instruction::TSX(AddressingMode::Implied, 2));
        map.insert(0x48, Instruction::PHA(AddressingMode::Implied, 3));
        map.insert(0x68, Instruction::PLA(AddressingMode::Implied, 4));
        map.insert(0x08, Instruction::PHP(AddressingMode::Implied, 3));
        map.insert(0x28, Instruction::PLP(AddressingMode::Implied, 4));
        // STX
        map.insert(0x86, Instruction::STX(AddressingMode::ZeroPage, 3));
        map.insert(0x96, Instruction::STX(AddressingMode::ZeroPageY, 4));
        map.insert(0x8E, Instruction::STX(AddressingMode::Absolute, 4));
        // STY
        map.insert(0x84, Instruction::STY(AddressingMode::ZeroPage, 3));
        map.insert(0x94, Instruction::STY(AddressingMode::ZeroPageX, 4));
        map.insert(0x8C, Instruction::STY(AddressingMode::Absolute, 4));

        map
    };
}

#[derive(Debug)]
pub enum Instruction {
    ADC(AddressingMode, usize),
    AND(AddressingMode, usize),
    ASL(AddressingMode, usize),
    BIT(AddressingMode, usize),
    BPL(AddressingMode, usize),
    BMI(AddressingMode, usize),
    BVC(AddressingMode, usize),
    BVS(AddressingMode, usize),
    BCC(AddressingMode, usize),
    BCS(AddressingMode, usize),
    BNE(AddressingMode, usize),
    BEQ(AddressingMode, usize),
    BRK(AddressingMode, usize),
    CMP(AddressingMode, usize),
    CPX(AddressingMode, usize),
    CPY(AddressingMode, usize),
    DEC(AddressingMode, usize),
    EOR(AddressingMode, usize),
    CLC(AddressingMode, usize),
    SEC(AddressingMode, usize),
    CLI(AddressingMode, usize),
    SEI(AddressingMode, usize),
    CLV(AddressingMode, usize),
    CLD(AddressingMode, usize),
    SED(AddressingMode, usize),
    INC(AddressingMode, usize),
    JMP(AddressingMode, usize),
    JSR(AddressingMode, usize),
    LDA(AddressingMode, usize),
    LDX(AddressingMode, usize),
    LDY(AddressingMode, usize),
    LSR(AddressingMode, usize),
    NOP(AddressingMode, usize),
    ORA(AddressingMode, usize),
    TAX(AddressingMode, usize),
    TXA(AddressingMode, usize),
    DEX(AddressingMode, usize),
    INX(AddressingMode, usize),
    TAY(AddressingMode, usize),
    TYA(AddressingMode, usize),
    DEY(AddressingMode, usize),
    INY(AddressingMode, usize),
    ROL(AddressingMode, usize),
    ROR(AddressingMode, usize),
    RTI(AddressingMode, usize),
    RTS(AddressingMode, usize),
    SBC(AddressingMode, usize),
    STA(AddressingMode, usize),
    TXS(AddressingMode, usize),
    TSX(AddressingMode, usize),
    PHA(AddressingMode, usize),
    PLA(AddressingMode, usize),
    PHP(AddressingMode, usize),
    PLP(AddressingMode, usize),
    STX(AddressingMode, usize),
    STY(AddressingMode, usize),
}

pub mod instruction_func {

    use super::*;

    /// Run an ADC instruction and return the number of cycles it took to complete the instruction
    pub fn adc(cpu: &mut CPU, memory: &Memory, mode: &AddressingMode, time: usize) -> usize {

        let (operand, extra_cycles) = match *mode {
            AddressingMode::Immediate | AddressingMode::ZeroPage | AddressingMode::ZeroPageX
            | AddressingMode::Absolute | AddressingMode::AbsoluteX | AddressingMode::AbsoluteY
            | AddressingMode::IndexedIndirect | AddressingMode::IndirectIndexed =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for ADC", *mode),
        };

        let op = {
            if let AddressingMode::Immediate = *mode {
                operand as u8
            } else {
                memory.read(operand)
            }
        };

        let (result_temp, carry1) = cpu.a.overflowing_add(op);

        let (result, carry2) = result_temp.overflowing_add(cpu.p.c as u8);
        let carry_bit = carry1 || carry2;

        cpu.p.z = result == 0;
        cpu.p.c = carry_bit;
        cpu.p.n = CPU::check_if_neg(result);
        // using overflowing_add on an unsigned 8-bit checks for overflow in bit 7, and as such works as the carry
        // bit. We need to use the value of A and the operand and compare to the result to see if there is signed
        // overflow (an invalid two's complement result)
        let (_, signed_overflow) = (cpu.a as i8).overflowing_add(op as i8); // TODO: is this the best way to do this?
        cpu.p.v = signed_overflow;

        cpu.a = result;
        let cycles = time + extra_cycles;
        cycles
    }

    pub fn lda(cpu: &mut CPU, memory: &Memory, mode: &AddressingMode, time: usize) -> usize {

        let (operand, extra_cycles) = match *mode {
            AddressingMode::Immediate | AddressingMode::ZeroPage | AddressingMode::ZeroPageX
            | AddressingMode::Absolute | AddressingMode::AbsoluteX | AddressingMode::AbsoluteY
            | AddressingMode::IndexedIndirect | AddressingMode::IndirectIndexed =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for LDA", *mode),
        };

        let result = {
            if let AddressingMode::Immediate = *mode {
                operand as u8
            } else {
                memory.read(operand)
            }
        };

        cpu.a = result;

        cpu.p.n =  CPU::check_if_neg(result);
        cpu.p.z = operand == 0;

        let cycles = time + extra_cycles;
        cycles
    }

    pub fn ldx(cpu: &mut CPU, memory: &Memory, mode: &AddressingMode, time: usize) -> usize {

        let (operand, extra_cycles) = match *mode {
            AddressingMode::Immediate | AddressingMode::ZeroPage | AddressingMode::ZeroPageY
            | AddressingMode::Absolute | AddressingMode::AbsoluteY =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for LDX", *mode),
        };

        let result = {
            if let AddressingMode::Immediate = *mode {
                operand as u8
            } else {
                memory.read(operand)
            }
        };

        cpu.x = result;

        cpu.p.n =  CPU::check_if_neg(result);
        cpu.p.z = operand == 0;

        let cycles = time + extra_cycles;
        cycles
    }

    pub fn ldy(cpu: &mut CPU, memory: &Memory, mode: &AddressingMode, time: usize) -> usize {

        let (operand, extra_cycles) = match *mode {
            AddressingMode::Immediate | AddressingMode::ZeroPage | AddressingMode::ZeroPageX
            | AddressingMode::Absolute | AddressingMode::AbsoluteX =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for LDY", *mode),
        };

        let result = {
            if let AddressingMode::Immediate = *mode {
                operand as u8
            } else {
                memory.read(operand)
            }
        };

        cpu.y = result;

        cpu.p.n =  CPU::check_if_neg(result);
        cpu.p.z = operand == 0;

        let cycles = time + extra_cycles;
        cycles
    }

    pub fn sta(cpu: &mut CPU, memory: &mut Memory, mode: &AddressingMode, time: usize) -> usize {
        
        let (operand, extra_cycles) = match *mode {
            AddressingMode::ZeroPage | AddressingMode::ZeroPageX | AddressingMode::Absolute
            | AddressingMode::AbsoluteX | AddressingMode::AbsoluteY | AddressingMode::IndexedIndirect
            | AddressingMode::IndirectIndexed =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for STA", *mode)
        };

        memory.write(operand, cpu.a);

        let cycles = time + extra_cycles;
        cycles
    }

    pub fn stx(cpu: &mut CPU, memory: &mut Memory, mode: &AddressingMode, time: usize) -> usize {
	
        let (operand, extra_cycles) = match *mode {
            AddressingMode::ZeroPage | AddressingMode::ZeroPageY | AddressingMode::Absolute =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for STA", *mode)
        };

        memory.write(operand, cpu.x);

        let cycles = time + extra_cycles;
        cycles
    }

    pub fn sty(cpu: &mut CPU, memory: &mut Memory, mode: &AddressingMode, time: usize) -> usize {
	
        let (operand, extra_cycles) = match *mode {
            AddressingMode::ZeroPage | AddressingMode::ZeroPageX | AddressingMode::Absolute =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for STA", *mode)
        };

        memory.write(operand, cpu.y);

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
    fn adc_imm_test_no_carry_no_overflow() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x69);
        mem.write(1, 0x20);
	cpu.a = 0x24;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::ADC(mode, time) = instr {
            let cycles = instruction_func::adc(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.a, 0x44);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
	    assert_eq!(cpu.p.c, false);
	    assert_eq!(cpu.p.v, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn adc_imm_test_zero() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x69);
        mem.write(1, 0xFF);
	cpu.a = 0x01;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::ADC(mode, time) = instr {
            let cycles = instruction_func::adc(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.a, 0x00);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, true);
            assert_eq!(cpu.p.n, false);
	    assert_eq!(cpu.p.c, true);
	    assert_eq!(cpu.p.v, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn adc_imm_test_neg() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x69);
        mem.write(1, 0x01);
	cpu.a = 0xFE;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::ADC(mode, time) = instr {
            let cycles = instruction_func::adc(&mut cpu, &mem, mode, *time); // will do 1 + -2
            assert_eq!(cpu.a, 0xFF);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, true);
	    assert_eq!(cpu.p.c, false);
	    assert_eq!(cpu.p.v, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn adc_imm_test_no_carry_overflow() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x69);
        mem.write(1, 0x50);
	cpu.a = 0x50;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::ADC(mode, time) = instr {
            let cycles = instruction_func::adc(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.a, 0xA0);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, true);
	    assert_eq!(cpu.p.c, false);
	    assert_eq!(cpu.p.v, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn adc_imm_test_carry_no_overflow() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x69);
        mem.write(1, 0x50);
	cpu.a = 0xD0;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::ADC(mode, time) = instr {
            let cycles = instruction_func::adc(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.a, 0x20);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
	    assert_eq!(cpu.p.c, true);
	    assert_eq!(cpu.p.v, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn adc_imm_test_carry_overflow() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x69);
        mem.write(1, 0xD0);
	cpu.a = 0x90;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::ADC(mode, time) = instr {
            let cycles = instruction_func::adc(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.a, 0x60);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
	    assert_eq!(cpu.p.c, true);
	    assert_eq!(cpu.p.v, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn ldx_imm_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xA2);
        mem.write(1, 0x44);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::LDX(mode, time) = instr {
            let cycles = instruction_func::ldx(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.x, 0x44);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn ldx_zp_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x0040, 0x44);  // M[0x0040] <- 0x44
        mem.write(0, 0xA6);
        mem.write(1, 0x40);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::LDX(mode, time) = instr {
            let cycles = instruction_func::ldx(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.x, 0x44);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn ldx_zpy_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x0040, 0x44);  // M[0x0040] <- 0x44
        mem.write(0, 0xB6);
        mem.write(1, 0x3F);
        cpu.y = 1;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::LDX(mode, time) = instr {
            let cycles = instruction_func::ldx(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.x, 0x44);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn ldx_abs_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x0140, 0x44);
        mem.write(0, 0xAE);
        mem.write(1, 0x40);
        mem.write(2, 0x01);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::LDX(mode, time) = instr {
            let cycles = instruction_func::ldx(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.x, 0x44);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn ldx_absy_test_same_page() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x0140, 0x44);
        mem.write(0, 0xBE);
        mem.write(1, 0x3F);
        mem.write(2, 0x01);
        cpu.y = 0x01;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::LDX(mode, time) = instr {
            let cycles = instruction_func::ldx(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.x, 0x44);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn ldx_absy_test_diff_page() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x0140, 0x44);
        mem.write(0, 0xBE);
        mem.write(1, 0xC1);
        mem.write(2, 0x00);
        cpu.y = 0x7F;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::LDX(mode, time) = instr {
            let cycles = instruction_func::ldx(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.x, 0x44);
            assert_eq!(cycles, 5);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn ldy_imm_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xA0);
        mem.write(1, 0x44);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::LDY(mode, time) = instr {
            let cycles = instruction_func::ldy(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.y, 0x44);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn ldy_zp_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x0040, 0x44);  // M[0x0040] <- 0x44
        mem.write(0, 0xA4);
        mem.write(1, 0x40);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::LDY(mode, time) = instr {
            let cycles = instruction_func::ldy(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.y, 0x44);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn ldy_zpx_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x0040, 0x44);  // M[0x0040] <- 0x44
        mem.write(0, 0xB4);
        mem.write(1, 0x3F);
        cpu.x = 1;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::LDY(mode, time) = instr {
            let cycles = instruction_func::ldy(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.y, 0x44);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn ldy_abs_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x0140, 0x44);
        mem.write(0, 0xAC);
        mem.write(1, 0x40);
        mem.write(2, 0x01);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::LDY(mode, time) = instr {
            let cycles = instruction_func::ldy(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.y, 0x44);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn ldy_absx_test_same_page() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x0140, 0x44);
        mem.write(0, 0xBC);
        mem.write(1, 0x3F);
        mem.write(2, 0x01);
        cpu.x = 0x01;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::LDY(mode, time) = instr {
            let cycles = instruction_func::ldy(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.y, 0x44);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn ldy_absx_test_diff_page() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x0140, 0x44);
        mem.write(0, 0xBC);
        mem.write(1, 0xC1);
        mem.write(2, 0x00);
        cpu.x = 0x7F;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::LDY(mode, time) = instr {
            let cycles = instruction_func::ldy(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.y, 0x44);
            assert_eq!(cycles, 5);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn lda_imm_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xA9);
        mem.write(1, 0x44);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::LDA(mode, time) = instr {
            let cycles = instruction_func::lda(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.a, 0x44);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn lda_zp_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x0040, 0x44);  // M[0x0040] <- 0x44
        mem.write(0, 0xA5); // LDA $40
        mem.write(1, 0x40);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::LDA(mode, time) = instr {
            let cycles = instruction_func::lda(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.a, 0x44);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn lda_zpx_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x0040, 0x44);  // M[0x0040] <- 0x44
        mem.write(0, 0xB5);
        mem.write(1, 0x3F);
        cpu.x = 1;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::LDA(mode, time) = instr {
            let cycles = instruction_func::lda(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.a, 0x44);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn lda_abs_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x0140, 0x44);
        mem.write(0, 0xAD);
        mem.write(1, 0x40);
        mem.write(2, 0x01);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::LDA(mode, time) = instr {
            let cycles = instruction_func::lda(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.a, 0x44);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn lda_absx_test_same_page() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x0140, 0x44);
        mem.write(0, 0xBD);
        mem.write(1, 0x3F);
        mem.write(2, 0x01);
        cpu.x = 0x01;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::LDA(mode, time) = instr {
            let cycles = instruction_func::lda(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.a, 0x44);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn lda_absx_test_diff_page() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x0140, 0x44);
        mem.write(0, 0xBD);
        mem.write(1, 0xC1);
        mem.write(2, 0x00);
        cpu.x = 0x7F;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::LDA(mode, time) = instr {
            let cycles = instruction_func::lda(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.a, 0x44);
            assert_eq!(cycles, 5);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn lda_absy_test_same_page() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x0140, 0x44);
        mem.write(0, 0xB9);
        mem.write(1, 0x3F);
        mem.write(2, 0x01);
        cpu.y = 0x01;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::LDA(mode, time) = instr {
            let cycles = instruction_func::lda(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.a, 0x44);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn lda_absy_test_diff_page() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x0140, 0x44);
        mem.write(0, 0xB9);
        mem.write(1, 0xC1);
        mem.write(2, 0x00);
        cpu.y = 0x7F;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::LDA(mode, time) = instr {
            let cycles = instruction_func::lda(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.a, 0x44);
            assert_eq!(cycles, 5);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn lda_idx_in_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x0140, 0x44);
        mem.write(0, 0xA1);
	mem.write(1, 0xFD);
	mem.write(0xFE, 0x40);
	mem.write(0xFF, 0x01);
	cpu.x = 1;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::LDA(mode, time) = instr {
            let cycles = instruction_func::lda(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.a, 0x44);
            assert_eq!(cycles, 6);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
	
    }

    #[test]
    fn lda_in_idx_same_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x0140, 0x44);
        mem.write(0, 0xB1);
	mem.write(1, 0xFE);
	mem.write(0xFE, 0x3F);
	mem.write(0xFF, 0x01);
	cpu.y = 1;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::LDA(mode, time) = instr {
            let cycles = instruction_func::lda(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.a, 0x44);
            assert_eq!(cycles, 5);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn lda_in_idx_diff_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x0140, 0x44);
        mem.write(0, 0xB1);
	mem.write(1, 0xFE);
	mem.write(0xFE, 0xC1);
	mem.write(0xFF, 0x00);
	cpu.y = 0x7F;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::LDA(mode, time) = instr {
            let cycles = instruction_func::lda(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.a, 0x44);
            assert_eq!(cycles, 6);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn lda_imm_zero_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xA9);
        mem.write(1, 0x00);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::LDA(mode, time) = instr {
            let cycles = instruction_func::lda(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.a, 0x00);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, true);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn lda_imm_neg_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xA9);
        mem.write(1, 0x80);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::LDA(mode, time) = instr {
            let cycles = instruction_func::lda(&mut cpu, &mem, mode, *time);
            assert_eq!(cpu.a, 0x80);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn sta_zp_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xA9); // LDA #$44
        mem.write(1, 0x44);
        mem.write(2, 0x85); // STA $40
        mem.write(3, 0x40);
        cpu.instruction_cycle(&mut mem).unwrap(); // execte the LDA instruction
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::STA(mode, time) = instr {
            let cycles = instruction_func::sta(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0x44);
            assert_eq!(mem.read(0x0040), 0x44);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        }
    }

    #[test]
    fn stx_zp_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x86); // STX $40
        mem.write(1, 0x40);
	cpu.x = 0x44;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::STX(mode, time) = instr {
            let cycles = instruction_func::stx(&mut cpu, &mut mem, mode, *time);
            assert_eq!(mem.read(0x0040), 0x44);
            assert_eq!(cycles, 3);
        }
    }

    #[test]
    fn sty_zp_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x84); // STY $40
        mem.write(1, 0x40);
	cpu.y = 0x44;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mem)).unwrap();
        if let Instruction::STY(mode, time) = instr {
            let cycles = instruction_func::sty(&mut cpu, &mut mem, mode, *time);
            assert_eq!(mem.read(0x0040), 0x44);
            assert_eq!(cycles, 3);
        }
    }
}
