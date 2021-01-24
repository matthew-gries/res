use crate::cpu::CPU;
use crate::main_memory::MainMemory;
use crate::memory::Memory;

use std::collections::HashMap;

#[derive(Debug, PartialEq)]
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
pub fn get_operand_using_addr_mode(mode: &AddressingMode, cpu: &mut CPU, memory: &mut MainMemory) -> (u16, usize) {

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
                if MainMemory::check_if_page_crossed(addr_old, addr) {
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
                if MainMemory::check_if_page_crossed(addr_old, addr) {
                    1
                } else {
                    0
                }
            };
            (addr, cycles)
        },
        AddressingMode::Indirect => {
            let low_byte_ind = cpu.read_byte_and_increment(memory);
            let high_byte_ind = cpu.read_byte_and_increment(memory);
            if low_byte_ind == 0xFF {
                log::warn!("Low-byte of indirect vector landed at end of page!");
            }
            let addr_ind = ((high_byte_ind as u16) << 8) | low_byte_ind as u16;

            let low_byte = memory.read(addr_ind);
            let high_byte = memory.read(addr_ind.overflowing_add(1).0);
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
                if MainMemory::check_if_page_crossed(addr_old, addr) {
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
        // INX
        map.insert(0xE8, Instruction::INX(AddressingMode::Implied, 2));
        // INY
        map.insert(0xC8, Instruction::INY(AddressingMode::Implied, 2));
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

    // Sign extend an unsigned 8 bit to an unsigned 16-bit, mostly a helper for branching instructions
    fn sign_extend(x: u8) -> u16 {
        if (x >> 7) == 1 {
            (x as u16) | 0xFF00
        } else {
            x as u16
        } 
    }

    /// Run an ADC instruction and return the number of cycles it took to complete the instruction
    pub fn adc(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {

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
        let (_, signed_overflow) = (cpu.a as i8).overflowing_add(op as i8); // TODO: is this the best way to do this
        cpu.p.v = signed_overflow;

        cpu.a = result;
        let cycles = time + extra_cycles;
        cycles
    }

    pub fn and(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        
        let (operand, extra_cycles) = match *mode {
            AddressingMode::Immediate | AddressingMode::ZeroPage | AddressingMode::ZeroPageX
            | AddressingMode::Absolute | AddressingMode::AbsoluteX | AddressingMode::AbsoluteY
            | AddressingMode::IndexedIndirect | AddressingMode::IndirectIndexed =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for AND", *mode),
        };

        let result = {
            if let AddressingMode::Immediate = *mode {
                operand as u8
            } else {
                memory.read(operand)
            }
        };

        cpu.a = cpu.a & result;

        cpu.p.z = cpu.a == 0;
        cpu.p.n = CPU::check_if_neg(cpu.a);

        let cycles = time + extra_cycles;
        cycles
    }

    pub fn asl(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        
        let (operand, _) = match *mode {
            AddressingMode::Accumulator | AddressingMode::ZeroPage | AddressingMode::ZeroPageX
            | AddressingMode::Absolute | AddressingMode::AbsoluteX =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for ASL", *mode),
        };

        let (old_val, new_val) = {
            if *mode == AddressingMode::Accumulator {
                let old = cpu.a;
                cpu.a = cpu.a << 1;
                (old, cpu.a)
            } else {
                let mut op = memory.read(operand);
                let old = op;
                op = op << 1;
                memory.write(operand, op);
                (old, op)
            }
        };

        cpu.p.c = ((old_val & 0x80) >> 7) != 0; // set to contents of old bit 7
        cpu.p.z = new_val == 0;
        cpu.p.n = CPU::check_if_neg(new_val);

        time
    }

    pub fn bcc(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        let (operand, _) = match *mode {
            AddressingMode::Relative =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for BCC", *mode),
        };

        let extra_cycles = {
            if !cpu.p.c {
                let old_pc = cpu.pc;
                let operand = sign_extend(operand as u8);
                let (new_pc, _) = cpu.pc.overflowing_add(operand);
                cpu.pc = new_pc;
                if MainMemory::check_if_page_crossed(old_pc, cpu.pc) {
                    2 
                } else {
                    1
                }
            } else {
                0 // no extra cycles
            }
        };

        let cycles = time + extra_cycles;
        cycles
    }

    pub fn bcs(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        let (operand, _) = match *mode {
            AddressingMode::Relative =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for BCS", *mode),
        };


        let extra_cycles = {
            if cpu.p.c {
                let old_pc = cpu.pc;
                let operand = sign_extend(operand as u8);
                let (new_pc, _) = cpu.pc.overflowing_add(operand);
                cpu.pc = new_pc;
                if MainMemory::check_if_page_crossed(old_pc, cpu.pc) {
                    2 
                } else {
                    1
                }
            } else {
                0 // no extra cycles
            }
        };

        let cycles = time + extra_cycles;
        cycles
    }

    pub fn beq(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        let (operand, _) = match *mode {
            AddressingMode::Relative =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for BEQ", *mode),
        };

        let extra_cycles = {
            if cpu.p.z {
                let old_pc = cpu.pc;
                let operand = sign_extend(operand as u8);
                let (new_pc, _) = cpu.pc.overflowing_add(operand);
                cpu.pc = new_pc;
                if MainMemory::check_if_page_crossed(old_pc, cpu.pc) {
                    2 
                } else {
                    1
                }
            } else {
                0 // no extra cycles
            }
        };

        let cycles = time + extra_cycles;
        cycles
    }

    pub fn bne(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        let (operand, _) = match *mode {
            AddressingMode::Relative =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for BNE", *mode),
        };

        let extra_cycles = {
            if !cpu.p.z {
                let old_pc = cpu.pc;
                let operand = sign_extend(operand as u8);
                let (new_pc, _) = cpu.pc.overflowing_add(operand);
                cpu.pc = new_pc;
                if MainMemory::check_if_page_crossed(old_pc, cpu.pc) {
                    2 
                } else {
                    1
                }
            } else {
                0 // no extra cycles
            }
        };

        let cycles = time + extra_cycles;
        cycles
    }

    pub fn bmi(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        let (operand, _) = match *mode {
            AddressingMode::Relative =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for BMI", *mode),
        };

        let extra_cycles = {
            if cpu.p.n {
                let old_pc = cpu.pc;
                let operand = sign_extend(operand as u8);
                let (new_pc, _) = cpu.pc.overflowing_add(operand);
                cpu.pc = new_pc;
                if MainMemory::check_if_page_crossed(old_pc, cpu.pc) {
                    2 
                } else {
                    1
                }
            } else {
                0 // no extra cycles
            }
        };

        let cycles = time + extra_cycles;
        cycles
    }

    pub fn bpl(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        let (operand, _) = match *mode {
            AddressingMode::Relative =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for BPL", *mode),
        };

        let extra_cycles = {
            if !cpu.p.n {
                let old_pc = cpu.pc;
                let operand = sign_extend(operand as u8);
                let (new_pc, _) = cpu.pc.overflowing_add(operand);
                cpu.pc = new_pc;
                if MainMemory::check_if_page_crossed(old_pc, cpu.pc) {
                    2 
                } else {
                    1
                }
            } else {
                0 // no extra cycles
            }
        };

        let cycles = time + extra_cycles;
        cycles
    }

    pub fn bvc(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        let (operand, _) = match *mode {
            AddressingMode::Relative =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for BVC", *mode),
        };

        let extra_cycles = {
            if !cpu.p.v {
                let old_pc = cpu.pc;
                let operand = sign_extend(operand as u8);
                let (new_pc, _) = cpu.pc.overflowing_add(operand);
                cpu.pc = new_pc;
                if MainMemory::check_if_page_crossed(old_pc, cpu.pc) {
                    2 
                } else {
                    1
                }
            } else {
                0 // no extra cycles
            }
        };

        let cycles = time + extra_cycles;
        cycles
    }

    pub fn bvs(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        let (operand, _) = match *mode {
            AddressingMode::Relative =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for BVS", *mode),
        };

        let extra_cycles = {
            if cpu.p.v {
                let old_pc = cpu.pc;
                let operand = sign_extend(operand as u8);
               let (new_pc, _) = cpu.pc.overflowing_add(operand);
                cpu.pc = new_pc;
                if MainMemory::check_if_page_crossed(old_pc, cpu.pc) {
                    2 
                } else {
                    1
                }
            } else {
                0 // no extra cycles
            }
        };

        let cycles = time + extra_cycles;
        cycles
    }

    pub fn bit(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {

        let (operand, _) = match *mode {
            AddressingMode::ZeroPage | AddressingMode::Absolute =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for BIT", *mode),
        };

        let result = memory.read(operand);

        cpu.p.z = (result & cpu.a) == 0;
        cpu.p.n = (result & 0x80 >> 7) != 0;
        cpu.p.v = (result & 0x40 >> 6) != 0;
        
        time
    }

    pub fn brk(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        let (_, _) = match *mode {
            AddressingMode::Implied =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for BRK", *mode),
        };

        if cpu.p.i {
            // If interrupt disable flag is set, don't respond to interrupt
            return 0;
        }

        let pc_low_byte = (cpu.pc & 0x00FF) as u8;
        let pc_high_byte = ((cpu.pc & 0xFF00) >> 8) as u8;
        cpu.p.b = true;
        cpu.p.i = true;
        let flags = cpu.p.as_u8();

        cpu.push_stack(memory, pc_low_byte);
        cpu.push_stack(memory, pc_high_byte);
        cpu.push_stack(memory, flags);

        let irq_vector = (((memory.read(0xFFFE) as u16) << 8) | memory.read(0xFFFF) as u16) as u16;

        cpu.pc = irq_vector;
        
        time
    }

    pub fn clc(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        let (_, _) = match *mode {
            AddressingMode::Implied =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for CLC", *mode),
        };

        cpu.p.c = false;
        
        time
    }

    pub fn cld(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        let (_, _) = match *mode {
            AddressingMode::Implied =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for CLD", *mode),
        };
        
        // nes does not use the decimal flag, so do nothing
        
        time
    }

    pub fn cli(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        let (_, _) = match *mode {
            AddressingMode::Implied =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for CLI", *mode),
        };
        
        cpu.p.i = false;
        
        time
    }

    pub fn clv(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        let (_, _) = match *mode {
            AddressingMode::Implied =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for CLV", *mode),
        };
        
        cpu.p.v = false;
        
        time
    }

    pub fn cmp(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        let (operand, extra_cycles) = match *mode {
            AddressingMode::Immediate | AddressingMode::ZeroPage | AddressingMode::ZeroPageX
            | AddressingMode::Absolute | AddressingMode::AbsoluteX | AddressingMode::AbsoluteY
            | AddressingMode::IndexedIndirect | AddressingMode::IndirectIndexed =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for CMP", *mode),
        };

        let op = {
            if let AddressingMode::Immediate = *mode {
                operand as u8
            } else {
                memory.read(operand)
            }
        };

        cpu.p.c = cpu.a >= op;
        cpu.p.z = cpu.a == op;
        cpu.p.n = CPU::check_if_neg(cpu.a.overflowing_sub(op).0);
        
        let cycles = time + extra_cycles;
        cycles
    }

    pub fn cpx(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        let (operand, _) = match *mode {
            AddressingMode::Immediate | AddressingMode::ZeroPage | AddressingMode::Absolute =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for CPX", *mode),
        };

        let op = {
            if let AddressingMode::Immediate = *mode {
                operand as u8
            } else {
                memory.read(operand)
            }
        };

        cpu.p.c = cpu.x >= op;
        cpu.p.z = cpu.x == op;
        cpu.p.n = CPU::check_if_neg(cpu.x.overflowing_sub(op).0);
        
        time
    }

    pub fn cpy(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        let (operand, _) = match *mode {
            AddressingMode::Immediate | AddressingMode::ZeroPage | AddressingMode::Absolute =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for CPY", *mode),
        };

        let op = {
            if let AddressingMode::Immediate = *mode {
                operand as u8
            } else {
                memory.read(operand)
            }
        };

        cpu.p.c = cpu.y >= op;
        cpu.p.z = cpu.y == op;
        cpu.p.n = CPU::check_if_neg(cpu.y.overflowing_sub(op).0);
        
        time
    }

    pub fn dec(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        
        let (operand, _) = match *mode {
            AddressingMode::ZeroPage | AddressingMode::ZeroPageX
                | AddressingMode::Absolute | AddressingMode::AbsoluteX =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for DEC", *mode),
        };

        let result = memory.read(operand);
        let (result, _) = result.overflowing_sub(1);
        memory.write(operand, result);

        cpu.p.n = CPU::check_if_neg(result);
        cpu.p.z = result == 0;

        time
    }

    pub fn dex(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        
        let (_, _) = match *mode {
            AddressingMode::Implied =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for DEX", *mode),
        };

        cpu.x = cpu.x.overflowing_sub(1).0;

        cpu.p.n = CPU::check_if_neg(cpu.x);
        cpu.p.z = cpu.x == 0;

        time
    }

    pub fn dey(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        
        let (_, _) = match *mode {
            AddressingMode::Implied =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for DEY", *mode),
        };

        cpu.y = cpu.y.overflowing_sub(1).0;

        cpu.p.n = CPU::check_if_neg(cpu.y);
        cpu.p.z = cpu.y == 0;

        time
    }

    pub fn eor(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        
        let (operand, extra_cycles) = match *mode {
            AddressingMode::Immediate | AddressingMode::ZeroPage | AddressingMode::ZeroPageX
            | AddressingMode::Absolute | AddressingMode::AbsoluteX | AddressingMode::AbsoluteY
            | AddressingMode::IndexedIndirect | AddressingMode::IndirectIndexed =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for EOR", *mode),
        };

        let result = {
            if let AddressingMode::Immediate = *mode {
                operand as u8
            } else {
                memory.read(operand)
            }
        };

        cpu.a = cpu.a ^ result;

        cpu.p.z = cpu.a == 0;
        cpu.p.n = CPU::check_if_neg(cpu.a);

        let cycles = time + extra_cycles;
        cycles
    }

    pub fn inc(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        
        let (operand, _) = match *mode {
            AddressingMode::ZeroPage | AddressingMode::ZeroPageX
                | AddressingMode::Absolute | AddressingMode::AbsoluteX =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for INC", *mode),
        };

        let result = memory.read(operand);
        let (result, _) = result.overflowing_add(1);
        memory.write(operand, result);

        cpu.p.n = CPU::check_if_neg(result);
        cpu.p.z = result == 0;

        time
    }

    pub fn inx(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        
        let (_, _) = match *mode {
            AddressingMode::Implied =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for INX", *mode),
        };

        cpu.x = cpu.x.overflowing_add(1).0;

        cpu.p.n = CPU::check_if_neg(cpu.x);
        cpu.p.z = cpu.x == 0;

        time
    }

    pub fn iny(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        
        let (_, _) = match *mode {
            AddressingMode::Implied =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for INY", *mode),
        };

        cpu.y = cpu.y.overflowing_add(1).0;

        cpu.p.n = CPU::check_if_neg(cpu.y);
        cpu.p.z = cpu.y == 0;

        time
    }

    pub fn jmp(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {

        let (operand, _) = match *mode {
            AddressingMode::Absolute | AddressingMode::Indirect =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mod {:?} for JMP", *mode),
        };

        cpu.pc = operand;

        time
    }

    pub fn jsr(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {

        let (operand, _) = match *mode {
            AddressingMode::Absolute =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mod {:?} for JSR", *mode),
        };

        let ret_addr = cpu.pc.overflowing_sub(1).0;
        let ret_addr_low_byte = (ret_addr & 0x00FF) as u8;
        let ret_addr_high_byte = ((ret_addr & 0xFF00) >> 8) as u8;
        cpu.push_stack(memory, ret_addr_low_byte);
        cpu.push_stack(memory, ret_addr_high_byte);

        cpu.pc = operand;

        time
    }

    pub fn lda(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {

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

    pub fn ldx(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {

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

    pub fn ldy(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {

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

    pub fn lsr(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        
        let (operand, _) = match *mode {
            AddressingMode::Accumulator | AddressingMode::ZeroPage | AddressingMode::ZeroPageX
            | AddressingMode::Absolute | AddressingMode::AbsoluteX =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for LSR", *mode),
        };

        let (old_val, new_val) = {
            if *mode == AddressingMode::Accumulator {
                let old = cpu.a;
                cpu.a = cpu.a >> 1;
                (old, cpu.a)
            } else {
                let mut op = memory.read(operand);
                let old = op;
                op = op >> 1;
                memory.write(operand, op);
                (old, op)
            }
        };

        cpu.p.c = old_val & 0x01 != 0; // set to contents of old bit 0
        cpu.p.z = new_val == 0;
        cpu.p.n = false; // bit 7 is always set to 0

        time
    }

    pub fn nop(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        let (_, _) = match *mode {
            AddressingMode::Implied =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for NOP", *mode),
        };

        time
    }

    pub fn ora(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        
        let (operand, extra_cycles) = match *mode {
            AddressingMode::Immediate | AddressingMode::ZeroPage | AddressingMode::ZeroPageX
            | AddressingMode::Absolute | AddressingMode::AbsoluteX | AddressingMode::AbsoluteY
            | AddressingMode::IndexedIndirect | AddressingMode::IndirectIndexed =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for ORA", *mode),
        };

        let result = {
            if let AddressingMode::Immediate = *mode {
                operand as u8
            } else {
                memory.read(operand)
            }
        };

        cpu.a = cpu.a | result;

        cpu.p.z = cpu.a == 0;
        cpu.p.n = CPU::check_if_neg(cpu.a);

        let cycles = time + extra_cycles;
        cycles
    }
   
    pub fn pha(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        
        let (_, _) = match *mode {
            AddressingMode::Implied =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for PHA", *mode),
        };

        cpu.push_stack(memory, cpu.a);

        time
    }

    pub fn php(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        
        let (_, _) = match *mode {
            AddressingMode::Implied =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for PHP", *mode),
        };

        cpu.push_stack(memory, cpu.p.as_u8());

        time
    }

    pub fn pla(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        
        let (_, _) = match *mode {
            AddressingMode::Implied =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for PLA", *mode),
        };

        let a = cpu.pop_stack(memory);

        cpu.a = a;
        cpu.p.n = CPU::check_if_neg(a);
        cpu.p.z = a == 0;

        time
    }

    pub fn plp(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        
        let (_, _) = match *mode {
            AddressingMode::Implied =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for PLP", *mode),
        };

        let p = cpu.pop_stack(memory);

        cpu.p.from_u8(p);

        time
    }

    pub fn rol(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        let (operand, _) = match *mode {
            AddressingMode::Accumulator | AddressingMode::ZeroPage | AddressingMode::ZeroPageX
            | AddressingMode::Absolute | AddressingMode::AbsoluteX =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for ROL", *mode),
        };

        let (old_val, new_val) = {
            if *mode == AddressingMode::Accumulator {
                let old = cpu.a;
                cpu.a = cpu.a << 1;
                cpu.a = cpu.a | (cpu.p.c as u8);
                (old, cpu.a)
            } else {
                let mut op = memory.read(operand);
                let old = op;
                op = op << 1;
                op = op | (cpu.p.c as u8);
                memory.write(operand, op);
                (old, op)
            }
        };

        cpu.p.c = ((old_val & 0x80) >> 7) != 0; // set to contents of old bit 7
        cpu.p.z = new_val == 0;
        cpu.p.n = CPU::check_if_neg(new_val);

        time
    }

    pub fn ror(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        
        let (operand, _) = match *mode {
            AddressingMode::Accumulator | AddressingMode::ZeroPage | AddressingMode::ZeroPageX
            | AddressingMode::Absolute | AddressingMode::AbsoluteX =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for ROR", *mode),
        };

        let (old_val, new_val) = {
            if *mode == AddressingMode::Accumulator {
                let old = cpu.a;
                cpu.a = cpu.a >> 1;
                cpu.a = cpu.a | ((cpu.p.c as u8) << 7);
                (old, cpu.a)
            } else {
                let mut op = memory.read(operand);
                let old = op;
                op = op >> 1;
                op = op | ((cpu.p.c as u8) << 7);
                memory.write(operand, op);
                (old, op)
            }
        };

        cpu.p.c = old_val & 0x01 != 0; // set to contents of old bit 0
        cpu.p.z = new_val == 0;
        cpu.p.n = CPU::check_if_neg(new_val);

        time
    }

    pub fn rti(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        
        let (_, _) = match *mode {
            AddressingMode::Implied =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for RTI", *mode),
        };

        let flags = cpu.pop_stack(memory);
        let pc_high = cpu.pop_stack(memory);
        let pc_low = cpu.pop_stack(memory);

        let pc = ((pc_high as u16) << 8) | (pc_low as u16);

        cpu.p.from_u8(flags);
        cpu.pc = pc;

        time
    }

    pub fn rts(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        
        let (_, _) = match *mode {
            AddressingMode::Implied =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for RTS", *mode),
        };

        let high_byte = cpu.pop_stack(memory);
        let low_byte = cpu.pop_stack(memory);
        let new_pc = ((high_byte as u16) << 8) | (low_byte as u16);
        let new_pc = new_pc + 1;

        cpu.pc = new_pc;

        time
    }

    pub fn sec(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {

        let (_, _) = match *mode {
            AddressingMode::Implied =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for SEC", *mode),
        };

        cpu.p.c = true;
        
        time
    }

    pub fn sed(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        let (_, _) = match *mode {
            AddressingMode::Implied =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for SED", *mode),
        };
        
        // nes does not use the decimal flag, so do nothing
        
        time
    }

    pub fn sei(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        let (_, _) = match *mode {
            AddressingMode::Implied =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for SEI", *mode),
        };
        
        cpu.p.i = true;
        
        time
    }

    pub fn sta(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        
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

    pub fn stx(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        
        let (operand, extra_cycles) = match *mode {
            AddressingMode::ZeroPage | AddressingMode::ZeroPageY | AddressingMode::Absolute =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for STX", *mode)
        };

        memory.write(operand, cpu.x);

        let cycles = time + extra_cycles;
        cycles
    }

    pub fn sty(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        
        let (operand, extra_cycles) = match *mode {
            AddressingMode::ZeroPage | AddressingMode::ZeroPageX | AddressingMode::Absolute =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for STY", *mode)
        };

        memory.write(operand, cpu.y);

        let cycles = time + extra_cycles;
        cycles
    }

    pub fn sbc(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {

        let (operand, extra_cycles) = match *mode {
            AddressingMode::Immediate | AddressingMode::ZeroPage | AddressingMode::ZeroPageX
            | AddressingMode::Absolute | AddressingMode::AbsoluteX | AddressingMode::AbsoluteY
            | AddressingMode::IndexedIndirect | AddressingMode::IndirectIndexed =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for SBC", *mode),
        };

        let op = {
            if let AddressingMode::Immediate = *mode {
                operand as u8
            } else {
                memory.read(operand)
            }
        };

        let (result_temp, carry1) = cpu.a.overflowing_sub(op);

        let (result, carry2) = result_temp.overflowing_sub(1 - cpu.p.c as u8);
        let carry_bit = carry1 || carry2;

        cpu.p.z = result == 0;
        cpu.p.c = carry_bit;
        cpu.p.n = CPU::check_if_neg(result);
        // using overflowing_add on an unsigned 8-bit checks for overflow in bit 7, and as such works as the carry
        // bit. We need to use the value of A and the operand and compare to the result to see if there is signed
        // overflow (an invalid two's complement result)
        let (_, signed_overflow) = (cpu.a as i8).overflowing_sub(op as i8); // TODO: is this the best way to do this
        cpu.p.v = signed_overflow;

        cpu.a = result;
        let cycles = time + extra_cycles;
        cycles
    }

    pub fn tax(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        
        let (_, _) = match *mode {
            AddressingMode::Implied =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for TAX", *mode),
        };

        cpu.x = cpu.a;
        cpu.p.z = cpu.x == 0;
        cpu.p.n = CPU::check_if_neg(cpu.x);

        time
    }

    pub fn tay(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        
        let (_, _) = match *mode {
            AddressingMode::Implied =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for TAY", *mode),
        };

        cpu.y = cpu.a;
        cpu.p.z = cpu.y == 0;
        cpu.p.n = CPU::check_if_neg(cpu.y);

        time
    }

    pub fn tsx(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        
        let (_, _) = match *mode {
            AddressingMode::Implied =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for TSX", *mode),
        };

        cpu.x = cpu.sp;
        cpu.p.z = cpu.x == 0;
        cpu.p.n = CPU::check_if_neg(cpu.x);

        time
    }

    pub fn txa(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        
        let (_, _) = match *mode {
            AddressingMode::Implied =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for TXA", *mode),
        };

        cpu.a = cpu.x;
        cpu.p.z = cpu.a == 0;
        cpu.p.n = CPU::check_if_neg(cpu.a);

        time
    }

    pub fn txs(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        
        let (_, _) = match *mode {
            AddressingMode::Implied =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for TXS", *mode),
        };

        cpu.sp = cpu.x;

        time
    }

    pub fn tya(cpu: &mut CPU, memory: &mut MainMemory, mode: &AddressingMode, time: usize) -> usize {
        
        let (_, _) = match *mode {
            AddressingMode::Implied =>
                get_operand_using_addr_mode(mode, cpu, memory),
            _ => panic!("Unsupported addressing mode {:?} for TYA", *mode),
        };

        cpu.a = cpu.y;
        cpu.p.z = cpu.a == 0;
        cpu.p.n = CPU::check_if_neg(cpu.a);

        time
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn generate_cpu_and_mem() -> (CPU, MainMemory) {
        (CPU::new(), MainMemory::new())
    }

    #[test]
    fn adc_imm_test_no_carry_no_overflow() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x69);
        mem.write(1, 0x20);
        cpu.a = 0x24;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::ADC(mode, time) = instr {
            let cycles = instruction_func::adc(&mut cpu, &mut mem, mode, *time);
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
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::ADC(mode, time) = instr {
            let cycles = instruction_func::adc(&mut cpu, &mut mem, mode, *time);
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
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::ADC(mode, time) = instr {
            let cycles = instruction_func::adc(&mut cpu, &mut mem, mode, *time); // will do 1 + -2
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
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::ADC(mode, time) = instr {
            let cycles = instruction_func::adc(&mut cpu, &mut mem, mode, *time);
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
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::ADC(mode, time) = instr {
            let cycles = instruction_func::adc(&mut cpu, &mut mem, mode, *time);
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
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::ADC(mode, time) = instr {
            let cycles = instruction_func::adc(&mut cpu, &mut mem, mode, *time);
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
    fn and_imm_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x29);
        mem.write(1, 0xF0);
        cpu.a = 0x1F;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::AND(mode, time) = instr {
            let cycles = instruction_func::and(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0x10);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn and_imm_zero_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x29);
        mem.write(1, 0xF0);
        cpu.a = 0x0F;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::AND(mode, time) = instr {
            let cycles = instruction_func::and(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0x00);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, true);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn and_imm_neg_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x29);
        mem.write(1, 0xF0);
        cpu.a = 0x8F;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::AND(mode, time) = instr {
            let cycles = instruction_func::and(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0x80);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn asl_acc_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x0A);
        cpu.a = 0x01;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::ASL(mode, time) = instr {
            let cycles = instruction_func::asl(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0x02);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.c, false);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn asl_acc_carry_and_zero_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x0A);
        cpu.a = 0x80;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::ASL(mode, time) = instr {
            let cycles = instruction_func::asl(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0x00);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.c, true);
            assert_eq!(cpu.p.z, true);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn asl_acc_neg_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x0A);
        cpu.a = 0x40;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::ASL(mode, time) = instr {
            let cycles = instruction_func::asl(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0x80);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.c, false);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn asl_abs_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x0E);
        mem.write(1, 0x40);
        mem.write(2, 0x01);
        mem.write(0x0140, 0x01);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::ASL(mode, time) = instr {
            let cycles = instruction_func::asl(&mut cpu, &mut mem, mode, *time);
            assert_eq!(mem.read(0x0140), 0x02);
            assert_eq!(cycles, 6);
            assert_eq!(cpu.p.c, false);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bcc_no_branch_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.c = true;
        mem.write(0, 0x90);
        mem.write(1, 0x20);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BCC(mode, time) = instr {
            let cycles = instruction_func::bcc(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc, 0x2);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bcc_pos_offset_same_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x90);
        mem.write(1, 0x20);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BCC(mode, time) = instr {
            let cycles = instruction_func::bcc(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.pc, 0x22);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bcc_pos_offset_diff_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.pc = 0xF0;
        mem.write(0xF0, 0x90);
        mem.write(0xF1, 0x20);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BCC(mode, time) = instr {
            let cycles = instruction_func::bcc(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc, 0x112);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bcc_neg_offset_same_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.pc = 0x7F01;
        mem.write(0x7F01, 0x90);
        mem.write(0x7F02, 0xFD);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BCC(mode, time) = instr {
            let cycles = instruction_func::bcc(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.pc, 0x7F00);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bcc_neg_offset_diff_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.pc = 0x7F00;
        mem.write(0x7F00, 0x90);
        mem.write(0x7F01, 0xFD);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BCC(mode, time) = instr {
            let cycles = instruction_func::bcc(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc, 0x7EFF);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bcs_no_branch_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.c = false;
        mem.write(0, 0xB0);
        mem.write(1, 0x20);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BCS(mode, time) = instr {
            let cycles = instruction_func::bcs(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc, 0x2);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bcs_pos_offset_same_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.c = true;
        mem.write(0, 0xB0);
        mem.write(1, 0x20);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BCS(mode, time) = instr {
            let cycles = instruction_func::bcs(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.pc, 0x22);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bcs_pos_offset_diff_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.c = true;
        cpu.pc = 0xF0;
        mem.write(0xF0, 0xB0);
        mem.write(0xF1, 0x20);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BCS(mode, time) = instr {
            let cycles = instruction_func::bcs(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc, 0x112);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bcs_neg_offset_same_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.c = true;
        cpu.pc = 0x7F01;
        mem.write(0x7F01, 0xB0);
        mem.write(0x7F02, 0xFD);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BCS(mode, time) = instr {
            let cycles = instruction_func::bcs(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.pc, 0x7F00);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bcs_neg_offset_diff_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.c = true;
        cpu.pc = 0x7F00;
        mem.write(0x7F00, 0xB0);
        mem.write(0x7F01, 0xFD);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BCS(mode, time) = instr {
            let cycles = instruction_func::bcs(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc, 0x7EFF);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn beq_no_branch_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.z = false;
        mem.write(0, 0xF0);
        mem.write(1, 0x20);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BEQ(mode, time) = instr {
            let cycles = instruction_func::beq(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc, 0x2);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn beq_pos_offset_same_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.z = true;
        mem.write(0, 0xF0);
        mem.write(1, 0x20);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BEQ(mode, time) = instr {
            let cycles = instruction_func::beq(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.pc, 0x22);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn beq_pos_offset_diff_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.z = true;
        cpu.pc = 0xF0;
        mem.write(0xF0, 0xF0);
        mem.write(0xF1, 0x20);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BEQ(mode, time) = instr {
            let cycles = instruction_func::beq(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc, 0x112);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn beq_neg_offset_same_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.z = true;
        cpu.pc = 0x7F01;
        mem.write(0x7F01, 0xF0);
        mem.write(0x7F02, 0xFD);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BEQ(mode, time) = instr {
            let cycles = instruction_func::beq(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.pc, 0x7F00);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn beq_neg_offset_diff_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.z = true;
        cpu.pc = 0x7F00;
        mem.write(0x7F00, 0xF0);
        mem.write(0x7F01, 0xFD);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BEQ(mode, time) = instr {
            let cycles = instruction_func::beq(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc, 0x7EFF);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bne_no_branch_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.z = true;
        mem.write(0, 0xD0);
        mem.write(1, 0x20);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BNE(mode, time) = instr {
            let cycles = instruction_func::bne(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc, 0x2);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bne_pos_offset_same_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.z = false;
        mem.write(0, 0xD0);
        mem.write(1, 0x20);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BNE(mode, time) = instr {
            let cycles = instruction_func::bne(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.pc, 0x22);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bne_pos_offset_diff_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.z = false;
        cpu.pc = 0xF0;
        mem.write(0xF0, 0xD0);
        mem.write(0xF1, 0x20);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BNE(mode, time) = instr {
            let cycles = instruction_func::bne(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc, 0x112);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bne_neg_offset_same_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.z = false;
        cpu.pc = 0x7F01;
        mem.write(0x7F01, 0xD0);
        mem.write(0x7F02, 0xFD);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BNE(mode, time) = instr {
            let cycles = instruction_func::bne(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.pc, 0x7F00);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bne_neg_offset_diff_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.z = false;
        cpu.pc = 0x7F00;
        mem.write(0x7F00, 0xD0);
        mem.write(0x7F01, 0xFD);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BNE(mode, time) = instr {
            let cycles = instruction_func::bne(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc, 0x7EFF);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bmi_no_branch_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.n = false;
        mem.write(0, 0x30);
        mem.write(1, 0x20);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BMI(mode, time) = instr {
            let cycles = instruction_func::bmi(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc, 0x2);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bmi_pos_offset_same_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.n = true;
        mem.write(0, 0x30);
        mem.write(1, 0x20);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BMI(mode, time) = instr {
            let cycles = instruction_func::bmi(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.pc, 0x22);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bmi_pos_offset_diff_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.n = true;
        cpu.pc = 0xF0;
        mem.write(0xF0, 0x30);
        mem.write(0xF1, 0x20);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BMI(mode, time) = instr {
            let cycles = instruction_func::bmi(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc, 0x112);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bmi_neg_offset_same_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.n = true;
        cpu.pc = 0x7F01;
        mem.write(0x7F01, 0x30);
        mem.write(0x7F02, 0xFD);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BMI(mode, time) = instr {
            let cycles = instruction_func::bmi(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.pc, 0x7F00);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bmi_neg_offset_diff_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.n = true;
        cpu.pc = 0x7F00;
        mem.write(0x7F00, 0x30);
        mem.write(0x7F01, 0xFD);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BMI(mode, time) = instr {
            let cycles = instruction_func::bmi(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc, 0x7EFF);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bpl_no_branch_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.n = true;
        mem.write(0, 0x10);
        mem.write(1, 0x20);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BPL(mode, time) = instr {
            let cycles = instruction_func::bpl(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc, 0x2);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bpl_pos_offset_same_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.n = false;
        mem.write(0, 0x10);
        mem.write(1, 0x20);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BPL(mode, time) = instr {
            let cycles = instruction_func::bpl(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.pc, 0x22);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bpl_pos_offset_diff_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.n = false;
        cpu.pc = 0xF0;
        mem.write(0xF0, 0x10);
        mem.write(0xF1, 0x20);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BPL(mode, time) = instr {
            let cycles = instruction_func::bpl(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc, 0x112);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bpl_neg_offset_same_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.n = false;
        cpu.pc = 0x7F01;
        mem.write(0x7F01, 0x10);
        mem.write(0x7F02, 0xFD);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BPL(mode, time) = instr {
            let cycles = instruction_func::bpl(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.pc, 0x7F00);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bpl_neg_offset_diff_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.n = false;
        cpu.pc = 0x7F00;
        mem.write(0x7F00, 0x10);
        mem.write(0x7F01, 0xFD);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BPL(mode, time) = instr {
            let cycles = instruction_func::bpl(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc, 0x7EFF);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bvc_no_branch_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.v = true;
        mem.write(0, 0x50);
        mem.write(1, 0x20);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BVC(mode, time) = instr {
            let cycles = instruction_func::bvc(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc, 0x2);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bvc_pos_offset_same_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.v = false;
        mem.write(0, 0x50);
        mem.write(1, 0x20);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BVC(mode, time) = instr {
            let cycles = instruction_func::bvc(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.pc, 0x22);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bvc_pos_offset_diff_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.v = false;
        cpu.pc = 0xF0;
        mem.write(0xF0, 0x50);
        mem.write(0xF1, 0x20);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BVC(mode, time) = instr {
            let cycles = instruction_func::bvc(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc, 0x112);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bvc_neg_offset_same_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.v = false;
        cpu.pc = 0x7F01;
        mem.write(0x7F01, 0x50);
        mem.write(0x7F02, 0xFD);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BVC(mode, time) = instr {
            let cycles = instruction_func::bvc(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.pc, 0x7F00);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bvc_neg_offset_diff_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.v = false;
        cpu.pc = 0x7F00;
        mem.write(0x7F00, 0x50);
        mem.write(0x7F01, 0xFD);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BVC(mode, time) = instr {
            let cycles = instruction_func::bvc(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc, 0x7EFF);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bvs_no_branch_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.v = false;
        mem.write(0, 0x70);
        mem.write(1, 0x20);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BVS(mode, time) = instr {
            let cycles = instruction_func::bvs(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc, 0x2);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bvs_pos_offset_same_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.v = true;
        mem.write(0, 0x70);
        mem.write(1, 0x20);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BVS(mode, time) = instr {
            let cycles = instruction_func::bvs(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.pc, 0x22);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bvs_pos_offset_diff_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.v = true;
        cpu.pc = 0xF0;
        mem.write(0xF0, 0x70);
        mem.write(0xF1, 0x20);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BVS(mode, time) = instr {
            let cycles = instruction_func::bvs(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc, 0x112);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bvs_neg_offset_same_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.v = true;
        cpu.pc = 0x7F01;
        mem.write(0x7F01, 0x70);
        mem.write(0x7F02, 0xFD);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BVS(mode, time) = instr {
            let cycles = instruction_func::bvs(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.pc, 0x7F00);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bvs_neg_offset_diff_page_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.p.v = true;
        cpu.pc = 0x7F00;
        mem.write(0x7F00, 0x70);
        mem.write(0x7F01, 0xFD);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BVS(mode, time) = instr {
            let cycles = instruction_func::bvs(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc, 0x7EFF);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn bit_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x24);
        mem.write(1, 0x40);
        mem.write(0x0040, 0xE1);
        cpu.a = 2;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BIT(mode, time) = instr {
            let cycles = instruction_func::bit(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.p.z, true);
            assert_eq!(cpu.p.n, true);
            assert_eq!(cpu.p.v, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }

    }

    #[test]
    fn brk_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.pc = 0x1234; // M[0x1234] = 0 => Break
        cpu.sp = 0xFF;
        cpu.p.n = true;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::BRK(mode, time) = instr {
            let cycles = instruction_func::brk(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 7);
            assert_eq!(cpu.p.b, true);
            assert_eq!(cpu.pc, 0x0000);
            assert_eq!(mem.read(0x01FF), 0x35);
            assert_eq!(mem.read(0x01FE), 0x12);
            assert_eq!(mem.read(0x01FD), 0x94);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
        
        
    }

    #[test]
    fn cmp_imm_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xC9);
        mem.write(1, 0x02);
        cpu.a = 0x03;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::CMP(mode, time) = instr {
            let cycles = instruction_func::cmp(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
            assert_eq!(cpu.p.c, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn cmp_imm_carry_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xC9);
        mem.write(1, 0x03);
        cpu.a = 0x04;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::CMP(mode, time) = instr {
            let cycles = instruction_func::cmp(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
            assert_eq!(cpu.p.c, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn cmp_imm_zero_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xC9);
        mem.write(1, 0x03);
        cpu.a = 0x03;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::CMP(mode, time) = instr {
            let cycles = instruction_func::cmp(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, true);
            assert_eq!(cpu.p.n, false);
            assert_eq!(cpu.p.c, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn cmp_imm_neg_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xC9);
        mem.write(1, 0x03);
        cpu.a = 0x02;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::CMP(mode, time) = instr {
            let cycles = instruction_func::cmp(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, true);
            assert_eq!(cpu.p.c, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn cpx_imm_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xE0);
        mem.write(1, 0x02);
        cpu.x = 0x03;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::CPX(mode, time) = instr {
            let cycles = instruction_func::cpx(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
            assert_eq!(cpu.p.c, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn cpx_imm_carry_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xE0);
        mem.write(1, 0x03);
        cpu.x = 0x04;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::CPX(mode, time) = instr {
            let cycles = instruction_func::cpx(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
            assert_eq!(cpu.p.c, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn cpx_imm_zero_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xE0);
        mem.write(1, 0x03);
        cpu.x = 0x03;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::CPX(mode, time) = instr {
            let cycles = instruction_func::cpx(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, true);
            assert_eq!(cpu.p.n, false);
            assert_eq!(cpu.p.c, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn cpx_imm_neg_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xE0);
        mem.write(1, 0x03);
        cpu.x = 0x02;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::CPX(mode, time) = instr {
            let cycles = instruction_func::cpx(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, true);
            assert_eq!(cpu.p.c, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn cpy_imm_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xC0);
        mem.write(1, 0x02);
        cpu.y = 0x03;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::CPY(mode, time) = instr {
            let cycles = instruction_func::cpy(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
            assert_eq!(cpu.p.c, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn cpy_imm_carry_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xC0);
        mem.write(1, 0x03);
        cpu.y = 0x04;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::CPY(mode, time) = instr {
            let cycles = instruction_func::cpy(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
            assert_eq!(cpu.p.c, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn cpy_imm_zero_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xC0);
        mem.write(1, 0x03);
        cpu.y = 0x03;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::CPY(mode, time) = instr {
            let cycles = instruction_func::cpy(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, true);
            assert_eq!(cpu.p.n, false);
            assert_eq!(cpu.p.c, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn cpy_imm_neg_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xC0);
        mem.write(1, 0x03);
        cpu.y = 0x02;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::CPY(mode, time) = instr {
            let cycles = instruction_func::cpy(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, true);
            assert_eq!(cpu.p.c, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn dec_zp_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x00FF, 2);
        mem.write(0, 0xC6);
        mem.write(1, 0xFF);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::DEC(mode, time) = instr {
            let cycles = instruction_func::dec(&mut cpu, &mut mem, mode, *time);
            assert_eq!(mem.read(0x00FF), 1);
            assert_eq!(cycles, 5);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn dec_zp_zero_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x00FF, 1);
        mem.write(0, 0xC6);
        mem.write(1, 0xFF);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::DEC(mode, time) = instr {
            let cycles = instruction_func::dec(&mut cpu, &mut mem, mode, *time);
            assert_eq!(mem.read(0x00FF), 0);
            assert_eq!(cycles, 5);
            assert_eq!(cpu.p.z, true);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn dec_zp_neg_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x00FF, 0x81);
        mem.write(0, 0xC6);
        mem.write(1, 0xFF);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::DEC(mode, time) = instr {
            let cycles = instruction_func::dec(&mut cpu, &mut mem, mode, *time);
            assert_eq!(mem.read(0x00FF), 0x80);
            assert_eq!(cycles, 5);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn dex_zp_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xCA);
        cpu.x = 2;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::DEX(mode, time) = instr {
            let cycles = instruction_func::dex(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.x, 1);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn dex_zp_zero_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xCA);
        cpu.x = 0x01;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::DEX(mode, time) = instr {
            let cycles = instruction_func::dex(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.x, 0);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, true);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn dex_zp_neg_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xCA);
        cpu.x = 0x81;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::DEX(mode, time) = instr {
            let cycles = instruction_func::dex(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.x, 0x80);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn dey_zp_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x88);
        cpu.y = 2;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::DEY(mode, time) = instr {
            let cycles = instruction_func::dey(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.y, 1);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn dey_zp_zero_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x88);
        cpu.y = 0x01;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::DEY(mode, time) = instr {
            let cycles = instruction_func::dey(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.y, 0);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, true);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn dey_zp_neg_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x88);
        cpu.y = 0x81;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::DEY(mode, time) = instr {
            let cycles = instruction_func::dey(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.y, 0x80);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn eor_imm_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x49);
        mem.write(1, 0xF0);
        cpu.a = 0x8F;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::EOR(mode, time) = instr {
            let cycles = instruction_func::eor(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0x7F);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn eor_imm_zero_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x49);
        mem.write(1, 0xFF);
        cpu.a = 0xFF;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::EOR(mode, time) = instr {
            let cycles = instruction_func::eor(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0x00);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, true);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn eor_imm_neg_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x49);
        mem.write(1, 0xF0);
        cpu.a = 0x0F;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::EOR(mode, time) = instr {
            let cycles = instruction_func::eor(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0xFF);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    
#[test]
    fn inc_zp_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x00FF, 1);
        mem.write(0, 0xE6);
        mem.write(1, 0xFF);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::INC(mode, time) = instr {
            let cycles = instruction_func::inc(&mut cpu, &mut mem, mode, *time);
            assert_eq!(mem.read(0x00FF), 2);
            assert_eq!(cycles, 5);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn inc_zp_zero_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x00FF, 0xFF);
        mem.write(0, 0xE6);
        mem.write(1, 0xFF);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::INC(mode, time) = instr {
            let cycles = instruction_func::inc(&mut cpu, &mut mem, mode, *time);
            assert_eq!(mem.read(0x00FF), 0);
            assert_eq!(cycles, 5);
            assert_eq!(cpu.p.z, true);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn inc_zp_neg_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x00FF, 0x7F);
        mem.write(0, 0xE6);
        mem.write(1, 0xFF);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::INC(mode, time) = instr {
            let cycles = instruction_func::inc(&mut cpu, &mut mem, mode, *time);
            assert_eq!(mem.read(0x00FF), 0x80);
            assert_eq!(cycles, 5);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn inx_zp_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xE8);
        cpu.x = 1;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::INX(mode, time) = instr {
            let cycles = instruction_func::inx(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.x, 2);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn inx_zp_zero_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xE8);
        cpu.x = 0xFF;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::INX(mode, time) = instr {
            let cycles = instruction_func::inx(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.x, 0);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, true);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn inx_zp_neg_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xE8);
        cpu.x = 0x7F;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::INX(mode, time) = instr {
            let cycles = instruction_func::inx(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.x, 0x80);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn iny_zp_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xC8);
        cpu.y = 1;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::INY(mode, time) = instr {
            let cycles = instruction_func::iny(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.y, 2);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn jmp_abs_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x4C);
        mem.write(1, 0x40);
        mem.write(2, 0x01);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::JMP(mode, time) = instr {
            let cycles = instruction_func::jmp(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.pc, 0x0140);
            assert_eq!(cycles, 3);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn jmp_ind_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x03FE, 0x40);
        mem.write(0x03FF, 0x01);
        mem.write(0, 0x6C);
        mem.write(1, 0xFE);
        mem.write(2, 0x03);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::JMP(mode, time) = instr {
            let cycles = instruction_func::jmp(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.pc, 0x0140);
            assert_eq!(cycles, 5);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn iny_zp_zero_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xC8);
        cpu.y = 0xFF;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::INY(mode, time) = instr {
            let cycles = instruction_func::iny(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.y, 0);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, true);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn iny_zp_neg_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xC8);
        cpu.y = 0x7F;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::INY(mode, time) = instr {
            let cycles = instruction_func::iny(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.y, 0x80);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn jsr_test() {
        
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0x0240, 0x20);
        mem.write(0x0241, 0x40);
        mem.write(0x0242, 0x01);
        cpu.pc = 0x0240;
        cpu.sp = 0xFF;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::JSR(mode, time) = instr {
            let cycles = instruction_func::jsr(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.pc, 0x0140);
            assert_eq!(mem.read(0x01FF), 0x42);
            assert_eq!(mem.read(0x01FE), 0x02);
            assert_eq!(cycles, 6);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn ldx_imm_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xA2);
        mem.write(1, 0x44);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LDX(mode, time) = instr {
            let cycles = instruction_func::ldx(&mut cpu, &mut mem, mode, *time);
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
        mem.write(0x0040, 0x44);  // M[0x0040] <- 0x4
        mem.write(0, 0xA6);
        mem.write(1, 0x40);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LDX(mode, time) = instr {
            let cycles = instruction_func::ldx(&mut cpu, &mut mem, mode, *time);
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
        mem.write(0x0040, 0x44);  // M[0x0040] <- 0x4
        mem.write(0, 0xB6);
        mem.write(1, 0x3F);
        cpu.y = 1;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LDX(mode, time) = instr {
            let cycles = instruction_func::ldx(&mut cpu, &mut mem, mode, *time);
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
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LDX(mode, time) = instr {
            let cycles = instruction_func::ldx(&mut cpu, &mut mem, mode, *time);
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
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LDX(mode, time) = instr {
            let cycles = instruction_func::ldx(&mut cpu, &mut mem, mode, *time);
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
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LDX(mode, time) = instr {
            let cycles = instruction_func::ldx(&mut cpu, &mut mem, mode, *time);
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
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LDY(mode, time) = instr {
            let cycles = instruction_func::ldy(&mut cpu, &mut mem, mode, *time);
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
        mem.write(0x0040, 0x44);  // M[0x0040] <- 0x4
        mem.write(0, 0xA4);
        mem.write(1, 0x40);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LDY(mode, time) = instr {
            let cycles = instruction_func::ldy(&mut cpu, &mut mem, mode, *time);
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
        mem.write(0x0040, 0x44);  // M[0x0040] <- 0x4;
        mem.write(0, 0xB4);
        mem.write(1, 0x3F);
        cpu.x = 1;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LDY(mode, time) = instr {
            let cycles = instruction_func::ldy(&mut cpu, &mut mem, mode, *time);
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
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LDY(mode, time) = instr {
            let cycles = instruction_func::ldy(&mut cpu, &mut mem, mode, *time);
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
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LDY(mode, time) = instr {
            let cycles = instruction_func::ldy(&mut cpu, &mut mem, mode, *time);
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
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LDY(mode, time) = instr {
            let cycles = instruction_func::ldy(&mut cpu, &mut mem, mode, *time);
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
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LDA(mode, time) = instr {
            let cycles = instruction_func::lda(&mut cpu, &mut mem, mode, *time);
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
        mem.write(0x0040, 0x44);  // M[0x0040] <- 0x4
        mem.write(0, 0xA5); // LDA 
        mem.write(1, 0x40);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LDA(mode, time) = instr {
            let cycles = instruction_func::lda(&mut cpu, &mut mem, mode, *time);
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
        mem.write(0x0040, 0x44);  // M[0x0040] <- 0x4
        mem.write(0, 0xB5);
        mem.write(1, 0x3F);
        cpu.x = 1;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LDA(mode, time) = instr {
            let cycles = instruction_func::lda(&mut cpu, &mut mem, mode, *time);
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
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LDA(mode, time) = instr {
            let cycles = instruction_func::lda(&mut cpu, &mut mem, mode, *time);
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
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LDA(mode, time) = instr {
            let cycles = instruction_func::lda(&mut cpu, &mut mem, mode, *time);
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
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LDA(mode, time) = instr {
            let cycles = instruction_func::lda(&mut cpu, &mut mem, mode, *time);
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
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LDA(mode, time) = instr {
            let cycles = instruction_func::lda(&mut cpu, &mut mem, mode, *time);
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
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LDA(mode, time) = instr {
            let cycles = instruction_func::lda(&mut cpu, &mut mem, mode, *time);
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
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LDA(mode, time) = instr {
            let cycles = instruction_func::lda(&mut cpu, &mut mem, mode, *time);
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
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LDA(mode, time) = instr {
            let cycles = instruction_func::lda(&mut cpu, &mut mem, mode, *time);
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
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LDA(mode, time) = instr {
            let cycles = instruction_func::lda(&mut cpu, &mut mem, mode, *time);
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
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LDA(mode, time) = instr {
            let cycles = instruction_func::lda(&mut cpu, &mut mem, mode, *time);
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
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LDA(mode, time) = instr {
            let cycles = instruction_func::lda(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0x80);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn lsr_acc_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x4A);
        cpu.a = 0x02;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LSR(mode, time) = instr {
            let cycles = instruction_func::lsr(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0x01);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.c, false);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn lsr_acc_carry_and_zero_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x4A);
        cpu.a = 0x01;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LSR(mode, time) = instr {
            let cycles = instruction_func::lsr(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0x00);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.c, true);
            assert_eq!(cpu.p.z, true);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn lsr_abs_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x4E);
        mem.write(1, 0x40);
        mem.write(2, 0x01);
        mem.write(0x0140, 0x02);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::LSR(mode, time) = instr {
            let cycles = instruction_func::lsr(&mut cpu, &mut mem, mode, *time);
            assert_eq!(mem.read(0x0140), 0x01);
            assert_eq!(cycles, 6);
            assert_eq!(cpu.p.c, false);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn ora_imm_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x09);
        mem.write(1, 0x7F);
        cpu.a = 0x00;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::ORA(mode, time) = instr {
            let cycles = instruction_func::ora(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0x7f);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn ora_imm_zero_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x09);
        mem.write(1, 0x00);
        cpu.a = 0x00;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::ORA(mode, time) = instr {
            let cycles = instruction_func::ora(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0x00);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, true);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn ora_imm_neg_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x09);
        mem.write(1, 0x0F);
        cpu.a = 0xF0;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::ORA(mode, time) = instr {
            let cycles = instruction_func::ora(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0xFF);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn rol_acc_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x2A);
        cpu.a = 0x01;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::ROL(mode, time) = instr {
            let cycles = instruction_func::rol(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0x02);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.c, false);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn rol_acc_carry_and_zero_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x2A);
        cpu.a = 0x80;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::ROL(mode, time) = instr {
            let cycles = instruction_func::rol(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0x00);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.c, true);
            assert_eq!(cpu.p.z, true);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn rol_acc_neg_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x2A);
        cpu.a = 0x40;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::ROL(mode, time) = instr {
            let cycles = instruction_func::rol(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0x80);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.c, false);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn rol_acc_with_carry_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x2A);
        cpu.p.c = true;
        cpu.a = 0x01;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::ROL(mode, time) = instr {
            let cycles = instruction_func::rol(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0x03);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.c, false);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn rol_abs_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x2E);
        mem.write(1, 0x40);
        mem.write(2, 0x01);
        mem.write(0x0140, 0x01);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::ROL(mode, time) = instr {
            let cycles = instruction_func::rol(&mut cpu, &mut mem, mode, *time);
            assert_eq!(mem.read(0x0140), 0x02);
            assert_eq!(cycles, 6);
            assert_eq!(cpu.p.c, false);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn ror_acc_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x6A);
        cpu.a = 0x02;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::ROR(mode, time) = instr {
            let cycles = instruction_func::ror(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0x01);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.c, false);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn ror_acc_with_carry_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x6A);
        cpu.a = 0x02;
        cpu.p.c = true;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::ROR(mode, time) = instr {
            let cycles = instruction_func::ror(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0x81);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.c, false);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn ror_acc_carry_and_zero_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x6A);
        cpu.a = 0x01;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::ROR(mode, time) = instr {
            let cycles = instruction_func::ror(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0x00);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.c, true);
            assert_eq!(cpu.p.z, true);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn ror_abs_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x6E);
        mem.write(1, 0x40);
        mem.write(2, 0x01);
        mem.write(0x0140, 0x02);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::ROR(mode, time) = instr {
            let cycles = instruction_func::ror(&mut cpu, &mut mem, mode, *time);
            assert_eq!(mem.read(0x0140), 0x01);
            assert_eq!(cycles, 6);
            assert_eq!(cpu.p.c, false);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn pha_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.sp = 0xFF;
        cpu.a = 0xBE;
        mem.write(0, 0x48);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::PHA(mode, time) = instr {
            let cycles = instruction_func::pha(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 3);
            assert_eq!(mem.read(0x01FF), 0xBE);
            assert_eq!(cpu.sp, 0xFE);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn php_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.sp = 0xFF;
        cpu.p.n = true;
        cpu.p.c = true;
        mem.write(0, 0x08);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::PHP(mode, time) = instr {
            let cycles = instruction_func::php(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 3);
            assert_eq!(mem.read(0x01FF), 0x81);
            assert_eq!(cpu.sp, 0xFE);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn pla_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.sp = 0xFE;
        mem.write(0x01FF, 0x7F);
        mem.write(0, 0x68);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::PLA(mode, time) = instr {
            let cycles = instruction_func::pla(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.a, 0x7F);
            assert_eq!(cpu.sp, 0xFF);
            assert_eq!(cpu.p.n, false);
            assert_eq!(cpu.p.z, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn pla_zero_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.sp = 0xFE;
        cpu.a = 0xFF;
        mem.write(0x01FF, 0x00);
        mem.write(0, 0x68);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::PLA(mode, time) = instr {
            let cycles = instruction_func::pla(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.a, 0x00);
            assert_eq!(cpu.sp, 0xFF);
            assert_eq!(cpu.p.n, false);
            assert_eq!(cpu.p.z, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
        
    }

    #[test]
    fn pla_neg_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.sp = 0xFE;
        mem.write(0x01FF, 0xFF);
        mem.write(0, 0x68);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::PLA(mode, time) = instr {
            let cycles = instruction_func::pla(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.a, 0xFF);
            assert_eq!(cpu.sp, 0xFF);
            assert_eq!(cpu.p.n, true);
            assert_eq!(cpu.p.z, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
        
    }

    #[test]
    fn plp_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        cpu.sp = 0xFE;
        mem.write(0x01FF, 0xFF);
        mem.write(0, 0x28);
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::PLP(mode, time) = instr {
            let cycles = instruction_func::plp(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.sp, 0xFF);
            assert_eq!(cpu.p.c, true);
            assert_eq!(cpu.p.z, true);
            assert_eq!(cpu.p.i, true);
            assert_eq!(cpu.p.d, true);
            assert_eq!(cpu.p.b, true);
            assert_eq!(cpu.p.v, true);
            assert_eq!(cpu.p.n, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn rts_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x60);
        mem.write(0x01FF, 0x42);
        mem.write(0x01FE, 0x02);
        cpu.sp = 0xFD;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::RTS(mode, time) = instr {
            let cycles = instruction_func::rts(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 6);
            assert_eq!(cpu.pc, 0x0243);
            assert_eq!(cpu.sp, 0xFF);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn rti_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x40);
        mem.write(0x01FF, 0x43);
        mem.write(0x01FE, 0x02);
        mem.write(0x01FD, 0xFF);
        cpu.sp = 0xFC;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::RTI(mode, time) = instr {
            let cycles = instruction_func::rti(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cycles, 6);
            assert_eq!(cpu.pc, 0x0243);
            assert_eq!(cpu.sp, 0xFF);
            assert_eq!(cpu.p.c, true);
            assert_eq!(cpu.p.z, true);
            assert_eq!(cpu.p.i, true);
            assert_eq!(cpu.p.d, true);
            assert_eq!(cpu.p.b, true);
            assert_eq!(cpu.p.v, true);
            assert_eq!(cpu.p.n, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn sta_zp_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xA9); // LDA #$4
        mem.write(1, 0x44);
        mem.write(2, 0x85); // STA $4
        mem.write(3, 0x40);
        cpu.instruction_cycle(&mut mem); // execte the LDA instruction
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
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
        mem.write(0, 0x86); // STX $4
        mem.write(1, 0x40);
        cpu.x = 0x44;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::STX(mode, time) = instr {
            let cycles = instruction_func::stx(&mut cpu, &mut mem, mode, *time);
            assert_eq!(mem.read(0x0040), 0x44);
            assert_eq!(cycles, 3);
        }
    }

    #[test]
    fn sty_zp_test() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0x84); // STY $4
        mem.write(1, 0x40);
        cpu.y = 0x44;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::STY(mode, time) = instr {
            let cycles = instruction_func::sty(&mut cpu, &mut mem, mode, *time);
            assert_eq!(mem.read(0x0040), 0x44);
            assert_eq!(cycles, 3);
        }
    }

    #[test]
    fn sbc_imm_test_no_carry_no_overflow() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xE9);
        mem.write(1, 0x30);
        cpu.a = 0x50;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::SBC(mode, time) = instr {
            let cycles = instruction_func::sbc(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0x1F);
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
    fn sbc_imm_test_zero() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xE9);
        mem.write(1, 0x01);
        cpu.a = 0x02;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::SBC(mode, time) = instr {
            let cycles = instruction_func::sbc(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0x00);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, true);
            assert_eq!(cpu.p.n, false);
            assert_eq!(cpu.p.c, false);
            assert_eq!(cpu.p.v, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn sbc_imm_test_neg() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xE9);
        mem.write(1, 0x30);
        cpu.a = 0xD0;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::SBC(mode, time) = instr {
            let cycles = instruction_func::sbc(&mut cpu, &mut mem, mode, *time); // will do 1 + -2
            assert_eq!(cpu.a, 0x9F);
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
    fn sbc_imm_test_no_carry_overflow() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xE9);
        mem.write(1, 0x70);
        cpu.a = 0xD0;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::SBC(mode, time) = instr {
            let cycles = instruction_func::sbc(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0x5F);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, false);
            assert_eq!(cpu.p.c, false);
            assert_eq!(cpu.p.v, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn sbc_imm_test_carry_no_overflow() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xE9);
        mem.write(1, 0xF0);
        cpu.a = 0xD0;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::SBC(mode, time) = instr {
            let cycles = instruction_func::sbc(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0xDF);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, true);
            assert_eq!(cpu.p.c, true);
            assert_eq!(cpu.p.v, false);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }

    #[test]
    fn sbc_imm_test_carry_overflow() {
        let (mut cpu, mut mem) = generate_cpu_and_mem();
        mem.write(0, 0xE9);
        mem.write(1, 0xB0);
        cpu.a = 0x50;
        let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut mem)).unwrap();
        if let Instruction::SBC(mode, time) = instr {
            let cycles = instruction_func::sbc(&mut cpu, &mut mem, mode, *time);
            assert_eq!(cpu.a, 0x9F);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.p.z, false);
            assert_eq!(cpu.p.n, true);
            assert_eq!(cpu.p.c, true);
            assert_eq!(cpu.p.v, true);
        } else {
            panic!("Wrong instruction, got {:?}", instr);
        }
    }
}
