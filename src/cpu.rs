use crate::instruction::Instruction;
use crate::instruction::instruction_func;
use crate::instruction::INSTRUCTION_TABLE;
use crate::memory::Memory;

/// Structure for handling the status register
pub struct StatusRegister {
    pub n: bool,
    pub v: bool,
    pub b: bool,
    pub d: bool,
    pub i: bool,
    pub z: bool,
    pub c: bool
}

impl StatusRegister {

    pub fn new() -> Self {
        StatusRegister{
                n: false,
                v: false,
                b: false,
                d: false,
                i: false,
                z: false,
                c: false}
    }

    pub fn as_u8(&self) -> u8 {
        let mut num: u8 = 0;
        num |= self.c as u8;
        num |= (self.z as u8) << 1;
        num |= (self.i as u8) << 2;
        num |= (self.d as u8) << 3;
        num |= (self.b as u8) << 4;
        num |= (self.v as u8) << 6;
        num |= (self.n as u8) << 7;
        num
    }

    pub fn from_u8(&mut self, flags: u8) {
        self.c = (flags & 0b1u8) != 0;
        self.z = (flags & 0b10u8) != 0;
        self.i = (flags & 0b100u8) != 0;
        self.d = (flags & 0b1000u8) != 0;
        self.b = (flags & 0b10000u8) != 0;
        self.v = (flags & 0b1000000u8) != 0;
        self.n = (flags & 0b10000000u8) != 0;
    }
}

/// The memory page in which the stack is located. The stack is located from
/// 0x0100 to 0x01FF
const STACK_MEMORY_PAGE: u16 = 0x0100;

/// Structure to represent the CPU of the system
pub struct CPU {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub sp: u8,
    pub pc: u16,
    pub p: StatusRegister
}

impl CPU {

    /// Create a new CPU with all fields zeroed
    pub fn new() -> Self {
        CPU{a: 0, x:0, y: 0, sp: 0, pc: 0, p: StatusRegister::new()}
    }

    /// Setup the CPU for use in the NES system (including setting the stack pointer)
    pub fn init(&mut self) -> &Self {
        self.sp = 0xFF;
        self
    }

    /// Push a value onto the stack
    pub fn push_stack(&mut self, memory: &mut Memory, val: u8) -> &Self {
        let stack_addr = STACK_MEMORY_PAGE + self.sp as u16;
        memory.write(stack_addr, val);
        let (new_stack_addr, underflowed) = self.sp.overflowing_sub(1);
        self.sp = new_stack_addr;
        if underflowed {
            log::warn!("Stack underflow detected");
        }
        self
    }

    /// Retrieve the value at the top of the stack
    pub fn pop_stack(&mut self, memory: &mut Memory) -> u8 {
        let (new_stack_addr, overflowed) = self.sp.overflowing_add(1);
        self.sp = new_stack_addr;
        if overflowed {
            log::warn!("Stack overflow detected");
        }
        let stack_addr = STACK_MEMORY_PAGE + self.sp as u16;
        memory.read(stack_addr) // value at the address is NOT deleted when SP moves
    }

    /// Check if the given unsigned value is negative in two's complement
    pub fn check_if_neg(val: u8) -> bool {
        (val >> 7) == 1
    }

    /// Get the byte at the PC and increment the PC by 1
    pub fn read_byte_and_increment(&mut self, memory: &Memory) -> u8 {
        let byte = memory.read(self.pc);
        self.pc += 1;
        byte
    }

    /// Perform one fetch-decode-execute cycle
    pub fn instruction_cycle(&mut self, memory: &mut Memory) -> Result<(), String> {
        let opcode = self.read_byte_and_increment(memory);
        let instr = INSTRUCTION_TABLE.get(&opcode);

        let instr = match instr {
            Some(i) => i,
            None => {
                return Err(format!("Invalid opcode {} was given!", opcode).to_string());
            }
        };

        let mut cycles = match instr {
            Instruction::ADC(mode, time) => instruction_func::adc(self, memory, mode, *time),
            Instruction::AND(mode, time) => instruction_func::and(self, memory, mode, *time),
            Instruction::ASL(mode, time) => instruction_func::asl(self, memory, mode, *time),
            Instruction::CLC(mode, time) => instruction_func::clc(self, memory, mode, *time),
            Instruction::CLD(mode, time) => instruction_func::cld(self, memory, mode, *time),
            Instruction::CLI(mode, time) => instruction_func::cli(self, memory, mode, *time),
            Instruction::CLV(mode, time) => instruction_func::clv(self, memory, mode, *time),
            Instruction::CMP(mode, time) => instruction_func::cmp(self, memory, mode, *time),
            Instruction::CPX(mode, time) => instruction_func::cpx(self, memory, mode, *time),
            Instruction::CPY(mode, time) => instruction_func::cpy(self, memory, mode, *time),
            Instruction::DEC(mode, time) => instruction_func::dec(self, memory, mode, *time),
            Instruction::DEX(mode, time) => instruction_func::dex(self, memory, mode, *time),
            Instruction::DEY(mode, time) => instruction_func::dey(self, memory, mode, *time),
            Instruction::EOR(mode, time) => instruction_func::eor(self, memory, mode, *time),
            Instruction::INC(mode, time) => instruction_func::inc(self, memory, mode, *time),
            Instruction::INX(mode, time) => instruction_func::inx(self, memory, mode, *time),
            Instruction::INY(mode, time) => instruction_func::iny(self, memory, mode, *time),
            Instruction::JMP(mode, time) => instruction_func::jmp(self, memory, mode, *time),
            Instruction::LDA(mode, time) => instruction_func::lda(self, memory, mode, *time),
            Instruction::LDX(mode, time) => instruction_func::ldx(self, memory, mode, *time),
            Instruction::LDY(mode, time) => instruction_func::ldy(self, memory, mode, *time),
            Instruction::LSR(mode, time) => instruction_func::lsr(self, memory, mode, *time),
            Instruction::NOP(mode, time) => instruction_func::nop(self, memory, mode, *time),
            Instruction::ORA(mode, time) => instruction_func::ora(self, memory, mode, *time),
            Instruction::PHA(mode, time) => instruction_func::pha(self, memory, mode, *time),
            Instruction::PHP(mode, time) => instruction_func::php(self, memory, mode, *time),
            Instruction::PLA(mode, time) => instruction_func::pla(self, memory, mode, *time),
            Instruction::PLP(mode, time) => instruction_func::plp(self, memory, mode, *time),
            Instruction::ROL(mode, time) => instruction_func::rol(self, memory, mode, *time),
            Instruction::ROR(mode, time) => instruction_func::ror(self, memory, mode, *time),
            Instruction::SBC(mode, time) => instruction_func::sbc(self, memory, mode, *time),
            Instruction::SEC(mode, time) => instruction_func::sec(self, memory, mode, *time),
            Instruction::SED(mode, time) => instruction_func::sed(self, memory, mode, *time),
            Instruction::SEI(mode, time) => instruction_func::sei(self, memory, mode, *time),
            Instruction::STA(mode, time) => instruction_func::sta(self, memory, mode, *time),
            Instruction::STX(mode, time) => instruction_func::stx(self, memory, mode, *time),
            Instruction::STY(mode, time) => instruction_func::sty(self, memory, mode, *time),
            Instruction::TAX(mode, time) => instruction_func::tax(self, memory, mode, *time),
            Instruction::TAY(mode, time) => instruction_func::tay(self, memory, mode, *time),
            Instruction::TSX(mode, time) => instruction_func::tsx(self, memory, mode, *time),
            Instruction::TXA(mode, time) => instruction_func::txa(self, memory, mode, *time),
            Instruction::TXS(mode, time) => instruction_func::txs(self, memory, mode, *time),
            Instruction::TYA(mode, time) => instruction_func::tya(self, memory, mode, *time),
            _ => panic!("{:?} has not been implemented!")
        };

        // loop the number of cycles it took to complete the instruction
        while cycles != 0 {
            cycles -= 1;
        }

        Ok(())
    }

}
