use crate::instruction::Instruction;
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
	    c: false
	}
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

    pub fn new() -> Self {
	CPU{a: 0, x:0, y: 0, sp: 0, pc: 0, p: StatusRegister::new()}
    }

    /// Get the byte at the PC and increment the PC by 1
    pub fn get_byte_and_increment(&mut self, memory: &Memory) -> u8 {
	let byte = memory.read(self.pc);
	self.pc += 1;
	byte
    }

    /// Fetch the current instruction in memory
    pub fn fetch_instruction(&mut self, memory: &Memory) -> Option<&Instruction> {
	let opcode = self.get_byte_and_increment(memory);
	let instr = INSTRUCTION_TABLE.get(&opcode);
	instr
    }
}
