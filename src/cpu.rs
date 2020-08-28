use crate::instruction::Instruction;
use crate::instruction::instruction_func;
use crate::instruction::INSTRUCTION_TABLE;
use crate::memory::Memory;
use crate::main_memory::MainMemory;

// Constants to represent the PPU registers in the main memory
const PPU_CONTROL_1: u16 = 0x2000;
const PPU_CONTROL_2: u16 = 0x2001;
const PPU_STATUS:    u16 = 0x2002;
const SPR_RAM_ADDR:  u16 = 0x2003;
const SPR_RAM_IO:    u16 = 0x2004;
const VRAM_ADDR_1:   u16 = 0x2005;
const VRAM_ADDR_2:   u16 = 0x2006;
const VRAM_ADDR_IO:  u16 = 0x2007;
const SPRITE_DMA:    u16 = 0x4014;


/// Structure for handling the status register of the CPU.
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

    /// Construct a new status register object.
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

    /// Convert the fields of the status register into an unsigned 8-bit number.
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

    /// Convert an unsigned 8-bit number into the flags of the status register.
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
/// 0x0100 to 0x01FF.
const STACK_MEMORY_PAGE: u16 = 0x0100;

/// Structure to represent the CPU of the NES system.
pub struct CPU {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub sp: u8,
    pub pc: u16,
    pub p: StatusRegister
}

impl CPU {

    /// Create a new CPU with all fields zeroed.
    pub fn new() -> Self {
        CPU{a: 0, x:0, y: 0, sp: 0, pc: 0, p: StatusRegister::new()}
    }

    /// Perform a reset interrupt request, which involves setting the program counter to the
    /// value stored in 0xFFFC and 0xFFFD.
    pub fn reset(&mut self, memory: &mut MainMemory) -> usize {

        // Values come from Visual6502 simulator.
        self.a = 0;
        self.x = 0;
        self.y = 0;
        self.sp = 0xFD;
        self.p.from_u8(16 as u8);

        // we know that these values are safe to read to, so just unwrap
        self.pc = ((memory.read(0xFFFD).unwrap() as u16) << 8) | memory.read(0xFFFC).unwrap() as u16;

        // resets take 8 cycles
        8
    }

    /// Perform an interrupt request, which involves pushing the low and high bytes of the PC on the stack, the
    /// processor flags on the stack, disabling interrupts and setting the PC to the value stored in 0xFFFE/0xFFFF.
    pub fn irq(&mut self, memory: &mut MainMemory) -> usize {

        if !self.p.i {
            let pc_low = (self.pc & 0x00FF) as u8;
            let pc_high = ((self.pc & 0xFF) >> 8) as u8;
            self.push_stack(memory, pc_low);
            self.push_stack(memory, pc_high);
            self.p.b = false;
            self.p.i = true;
            self.push_stack(memory, self.p.as_u8());

            let pc_low = memory.read(0xFFFE).unwrap();
            let pc_high = memory.read(0xFFFF).unwrap();

            self.pc = ((pc_high as u16) << 8) | pc_low as u16;

            // IRQ take 7 cycles
            7
        } else {
            // if we ignored the request, we didn't use any cycles
            0
        }
    }

    /// Perform a non-maskable interrupt, which involves pushing the low and high bytes of the PC on the stack, the
    /// processor flags on the stack, disabling interrupts and setting the PC to the value stored in 0xFFFA/0xFFFB.
    pub fn nmi(&mut self, memory: &mut MainMemory) -> usize {

        let pc_low = (self.pc & 0x00FF) as u8;
        let pc_high = ((self.pc & 0xFF) >> 8) as u8;
        self.push_stack(memory, pc_low);
        self.push_stack(memory, pc_high);
        self.push_stack(memory, self.p.as_u8());

        self.p.i = true;
        self.p.b = false;

        let pc_low = memory.read(0xFFFA).unwrap();
        let pc_high = memory.read(0xFFFB).unwrap();

        self.pc = ((pc_high as u16) << 8) | pc_low as u16;

        // NMIs take 7 cycles
        7
    }

    /// Push a value onto the stack.
    pub fn push_stack(&mut self, memory: &mut MainMemory, val: u8) -> &Self {
        let stack_addr = STACK_MEMORY_PAGE + self.sp as u16;
	// writing to the stack is always valid, so unwrap
        memory.write(stack_addr, val).unwrap();
        let (new_stack_addr, underflowed) = self.sp.overflowing_sub(1);
        self.sp = new_stack_addr;
        if underflowed {
            log::warn!("Stack underflow detected");
        }
        self
    }

    /// Retrieve the value at the top of the stack.
    pub fn pop_stack(&mut self, memory: &mut MainMemory) -> u8 {
        let (new_stack_addr, overflowed) = self.sp.overflowing_add(1);
        self.sp = new_stack_addr;
        if overflowed {
            log::warn!("Stack overflow detected");
        }
        let stack_addr = STACK_MEMORY_PAGE + self.sp as u16;
	// we know reading from the stack is aways valid, so just unwrap
        memory.read(stack_addr).unwrap() // value at the address is NOT deleted when SP moves
    }

    /// Check if the given unsigned value is negative in two's complement.
    pub fn check_if_neg(val: u8) -> bool {
        (val >> 7) == 1
    }

    /// Get the byte at the PC and increment the PC by 1.
    pub fn read_byte_and_increment(&mut self, memory: &mut MainMemory) -> Result<u8, &'static str> {
        let byte = memory.read(self.pc)?;
        self.pc += 1;
        Ok(byte)
    }

    /// Perform one fetch-decode-execute cycle.
    pub fn instruction_cycle(&mut self, memory: &mut MainMemory) -> Result<(), String> {
        println!("{:x}", self.pc);
        let opcode = match self.read_byte_and_increment(memory) {
            Ok(op) => op,
            Err(e) => {
            return Err(String::from(e));
            }
        };
	
        let instr = INSTRUCTION_TABLE.get(&opcode);

        let instr = match instr {
            Some(i) => i,
            None => {
                return Err(format!("Invalid opcode {} was given!", opcode).to_string());
            }
        };

        let cycles = match instr {
            Instruction::ADC(mode, time) => instruction_func::adc(self, memory, mode, *time),
            Instruction::AND(mode, time) => instruction_func::and(self, memory, mode, *time),
            Instruction::ASL(mode, time) => instruction_func::asl(self, memory, mode, *time),
            Instruction::BCC(mode, time) => instruction_func::bcc(self, memory, mode, *time),
            Instruction::BCS(mode, time) => instruction_func::bcs(self, memory, mode, *time),
            Instruction::BEQ(mode, time) => instruction_func::beq(self, memory, mode, *time),
            Instruction::BNE(mode, time) => instruction_func::bne(self, memory, mode, *time),
            Instruction::BMI(mode, time) => instruction_func::bmi(self, memory, mode, *time),
            Instruction::BPL(mode, time) => instruction_func::bpl(self, memory, mode, *time),
            Instruction::BVC(mode, time) => instruction_func::bvc(self, memory, mode, *time),
            Instruction::BVS(mode, time) => instruction_func::bvs(self, memory, mode, *time),
            Instruction::BIT(mode, time) => instruction_func::bit(self, memory, mode, *time),
            Instruction::BRK(mode, time) => instruction_func::brk(self, memory, mode, *time),
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
            Instruction::JSR(mode, time) => instruction_func::jsr(self, memory, mode, *time),
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
            Instruction::RTI(mode, time) => instruction_func::rti(self, memory, mode, *time),
            Instruction::RTS(mode, time) => instruction_func::rts(self, memory, mode, *time),
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
        };

        let mut cycles = cycles?;

        // loop the number of cycles it took to complete the instruction
        while cycles != 0 {
            cycles -= 1;
        }

        Ok(())
    }
}
