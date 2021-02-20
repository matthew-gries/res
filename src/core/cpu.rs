use crate::core::bus::CPUBus;
use crate::core::instruction::instruction_func;
use crate::core::instruction::Instruction;
use crate::core::instruction::INSTRUCTION_TABLE;

/// Structure for handling the status register of the CPU
pub struct StatusRegister {
    /// The negative flag (bit 7)
    pub n: bool,
    /// The overflow flag (bit 6)
    pub v: bool,
    /// The break command flag (bit 4)
    pub b: bool,
    /// The decimal mode flag (unused in NES system, bit 3)
    pub d: bool,
    /// The interrupt disable flag (bit 2)
    pub i: bool,
    /// The zero flag (bit 1)
    pub z: bool,
    /// The carry flag (bit 0)
    pub c: bool,
}

impl StatusRegister {
    /// Construct a new status register object
    ///
    /// Return (`StatusRegister`): the new status register object
    pub fn new() -> Self {
        StatusRegister {
            n: false,
            v: false,
            b: false,
            d: false,
            i: false,
            z: false,
            c: false,
        }
    }

    /// Convert the fields of the status register into an unsigned 8-bit number
    ///
    /// Return (`u8`): this status register object as an 8-bit number, with each bit corresponding to a
    /// flag
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
    /// Bit 5 of the number is unused
    ///
    /// Arguments:
    /// * `flags` (`u8`): the 8-bit number to use
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

/// The bus page in which the stack is located. The stack is located from
/// 0x0100 to 0x01FF
const STACK_MEM_PAGE: u16 = 0x0100;

/// Structure to represent the CPU of the NES system
pub struct CPU {
    /// The accumulator register
    pub a: u8,
    /// The X register
    pub x: u8,
    /// The Y register
    pub y: u8,
    /// The stack pointer
    pub sp: u8,
    /// The program counter
    pub pc: u16,
    /// The status register
    pub p: StatusRegister,
    /// `true` if the PPU is already in VBlank, `false` if the PPU is not already in VBlank
    is_ppu_already_in_vblank: bool,
}

impl CPU {
    /// Create a new CPU with all fields zeroed
    ///
    /// Return (`CPU`): a new CPU object
    pub fn new() -> Self {
        CPU {
            a: 0,
            x: 0,
            y: 0,
            sp: 0,
            pc: 0,
            p: StatusRegister::new(),
            is_ppu_already_in_vblank: false,
        }
    }

    /// Perform a reset interrupt request, which involves setting the program counter to the
    /// value stored in 0xFFFC and 0xFFFD
    ///
    /// Arguments:
    /// * `bus` (`CPUBus`): the system bus to use with this CPU
    ///
    /// Return (`u8`): the number of cycles this operation takes
    pub fn reset(&mut self, bus: &mut CPUBus) -> usize {
        // Values come from Visual6502 simulator.
        self.a = 0;
        self.x = 0;
        self.y = 0;
        self.sp = 0xFD;
        self.p.from_u8(16 as u8);

        self.pc = ((bus.read(0xFFFD) as u16) << 8) | bus.read(0xFFFC) as u16;

        // resets take 8 cycles
        8
    }

    /// Perform an interrupt request, which involves pushing the low and high bytes of the PC on the stack, the
    /// processor flags on the stack, disabling interrupts and setting the PC to the value stored in 0xFFFE/0xFFFF
    ///
    /// Arguments:
    /// * `bus` (`CPUBus`): the system bus to use with this CPU
    ///
    /// Return (`u8`): the number of cycles this operation takes
    pub fn irq(&mut self, bus: &mut CPUBus) -> usize {
        if !self.p.i {
            let pc_low = (self.pc & 0x00FF) as u8;
            let pc_high = ((self.pc & 0xFF) >> 8) as u8;
            self.push_stack(bus, pc_low);
            self.push_stack(bus, pc_high);
            self.p.b = false;
            self.p.i = true;
            self.push_stack(bus, self.p.as_u8());

            let pc_low = bus.read(0xFFFE);
            let pc_high = bus.read(0xFFFF);

            self.pc = ((pc_high as u16) << 8) | pc_low as u16;

            // IRQ take 7 cycles
            7
        } else {
            // if we ignored the request, we didn't use any cycles
            0
        }
    }

    /// Perform a non-maskable interrupt, which involves pushing the low and high bytes of the PC on the stack, the
    /// processor flags on the stack, disabling interrupts and setting the PC to the value stored in 0xFFFA/0xFFFB
    ///
    /// Arguments:
    /// * `bus` (`CPUBus`): the system bus to use with this CPU
    ///
    /// Return (`u8`): the number of cycles this operation takes
    pub fn nmi(&mut self, bus: &mut CPUBus) -> usize {
        let pc_low = (self.pc & 0x00FF) as u8;
        let pc_high = ((self.pc & 0xFF) >> 8) as u8;
        self.push_stack(bus, pc_low);
        self.push_stack(bus, pc_high);
        self.push_stack(bus, self.p.as_u8());

        self.p.i = true;
        self.p.b = false;

        let pc_low = bus.read(0xFFFA);
        let pc_high = bus.read(0xFFFB);

        self.pc = ((pc_high as u16) << 8) | pc_low as u16;

        // NMIs take 7 cycles
        7
    }

    /// Push a value onto the stack
    ///
    /// Argument:
    /// * `bus` (`CPUBus`): the system bus to use with this CPU
    /// * `val` (`u8`): the value to push on the stack
    ///
    /// Return (`&Self`): a reference to this CPU
    pub fn push_stack(&mut self, bus: &mut CPUBus, val: u8) -> &Self {
        let stack_addr = STACK_MEM_PAGE + self.sp as u16;

        bus.write(stack_addr, val);

        let (new_stack_addr, underflowed) = self.sp.overflowing_sub(1);
        self.sp = new_stack_addr;

        if underflowed {
            log::warn!("Stack underflow detected");
        }

        self
    }

    /// Retrieve the value at the top of the stack. The value is not deleted from
    /// bus
    ///
    /// Arguments:
    /// * `bus` (`CPUBus`): the system bus to use with this CPU
    ///
    /// Return (`u8`): the value popped from the stack
    pub fn pop_stack(&mut self, bus: &mut CPUBus) -> u8 {
        let (new_stack_addr, overflowed) = self.sp.overflowing_add(1);
        self.sp = new_stack_addr;
        if overflowed {
            log::warn!("Stack overflow detected");
        }
        let stack_addr = STACK_MEM_PAGE + self.sp as u16;

        // we know reading from the stack is aways valid, so just unwrap
        bus.read(stack_addr) // value at the address is NOT deleted when SP moves
    }

    /// Check if the given unsigned value is negative in two's complement
    ///
    /// Arguments:
    /// * `val` (`u8`): the value to check
    ///
    /// Return (`bool`): `true` if the value is negative in two's complement, `false` otherwise.
    pub fn check_if_neg(val: u8) -> bool {
        (val >> 7) == 1
    }

    /// Get the byte at the PC and increment the PC by 1
    ///
    /// Arguments:
    /// * `bus` (`CPUBus`): the system bus to use with this CPU
    ///
    /// Return (`u8`): the byte read at the address
    pub fn read_byte_and_increment(&mut self, bus: &mut CPUBus) -> u8 {
        let byte = bus.read(self.pc);
        self.pc += 1;
        byte
    }

    /// Perform one fetch-decode-execute cycle
    ///
    /// Arguments:
    /// * `bus` (`CPUBus`): the system bus to use with this CPU
    pub fn instruction_cycle(&mut self, bus: &mut CPUBus) {
        self.check_ppu_status_and_update(bus);

        let opcode = self.read_byte_and_increment(bus);

        let instr = INSTRUCTION_TABLE.get(&opcode);

        println!("{:x}: {:x}", self.pc, opcode);

        let instr = match instr {
            Some(i) => i,
            None => panic!("Invalid opcode {} was given!", opcode),
        };

        let mut cycles = match instr {
            Instruction::ADC(mode, time) => instruction_func::adc(self, bus, mode, *time),
            Instruction::AND(mode, time) => instruction_func::and(self, bus, mode, *time),
            Instruction::ASL(mode, time) => instruction_func::asl(self, bus, mode, *time),
            Instruction::BCC(mode, time) => instruction_func::bcc(self, bus, mode, *time),
            Instruction::BCS(mode, time) => instruction_func::bcs(self, bus, mode, *time),
            Instruction::BEQ(mode, time) => instruction_func::beq(self, bus, mode, *time),
            Instruction::BNE(mode, time) => instruction_func::bne(self, bus, mode, *time),
            Instruction::BMI(mode, time) => instruction_func::bmi(self, bus, mode, *time),
            Instruction::BPL(mode, time) => instruction_func::bpl(self, bus, mode, *time),
            Instruction::BVC(mode, time) => instruction_func::bvc(self, bus, mode, *time),
            Instruction::BVS(mode, time) => instruction_func::bvs(self, bus, mode, *time),
            Instruction::BIT(mode, time) => instruction_func::bit(self, bus, mode, *time),
            Instruction::BRK(mode, time) => instruction_func::brk(self, bus, mode, *time),
            Instruction::CLC(mode, time) => instruction_func::clc(self, bus, mode, *time),
            Instruction::CLD(mode, time) => instruction_func::cld(self, bus, mode, *time),
            Instruction::CLI(mode, time) => instruction_func::cli(self, bus, mode, *time),
            Instruction::CLV(mode, time) => instruction_func::clv(self, bus, mode, *time),
            Instruction::CMP(mode, time) => instruction_func::cmp(self, bus, mode, *time),
            Instruction::CPX(mode, time) => instruction_func::cpx(self, bus, mode, *time),
            Instruction::CPY(mode, time) => instruction_func::cpy(self, bus, mode, *time),
            Instruction::DEC(mode, time) => instruction_func::dec(self, bus, mode, *time),
            Instruction::DEX(mode, time) => instruction_func::dex(self, bus, mode, *time),
            Instruction::DEY(mode, time) => instruction_func::dey(self, bus, mode, *time),
            Instruction::EOR(mode, time) => instruction_func::eor(self, bus, mode, *time),
            Instruction::INC(mode, time) => instruction_func::inc(self, bus, mode, *time),
            Instruction::INX(mode, time) => instruction_func::inx(self, bus, mode, *time),
            Instruction::INY(mode, time) => instruction_func::iny(self, bus, mode, *time),
            Instruction::JMP(mode, time) => instruction_func::jmp(self, bus, mode, *time),
            Instruction::JSR(mode, time) => instruction_func::jsr(self, bus, mode, *time),
            Instruction::LDA(mode, time) => instruction_func::lda(self, bus, mode, *time),
            Instruction::LDX(mode, time) => instruction_func::ldx(self, bus, mode, *time),
            Instruction::LDY(mode, time) => instruction_func::ldy(self, bus, mode, *time),
            Instruction::LSR(mode, time) => instruction_func::lsr(self, bus, mode, *time),
            Instruction::NOP(mode, time) => instruction_func::nop(self, bus, mode, *time),
            Instruction::ORA(mode, time) => instruction_func::ora(self, bus, mode, *time),
            Instruction::PHA(mode, time) => instruction_func::pha(self, bus, mode, *time),
            Instruction::PHP(mode, time) => instruction_func::php(self, bus, mode, *time),
            Instruction::PLA(mode, time) => instruction_func::pla(self, bus, mode, *time),
            Instruction::PLP(mode, time) => instruction_func::plp(self, bus, mode, *time),
            Instruction::ROL(mode, time) => instruction_func::rol(self, bus, mode, *time),
            Instruction::ROR(mode, time) => instruction_func::ror(self, bus, mode, *time),
            Instruction::RTI(mode, time) => instruction_func::rti(self, bus, mode, *time),
            Instruction::RTS(mode, time) => instruction_func::rts(self, bus, mode, *time),
            Instruction::SBC(mode, time) => instruction_func::sbc(self, bus, mode, *time),
            Instruction::SEC(mode, time) => instruction_func::sec(self, bus, mode, *time),
            Instruction::SED(mode, time) => instruction_func::sed(self, bus, mode, *time),
            Instruction::SEI(mode, time) => instruction_func::sei(self, bus, mode, *time),
            Instruction::STA(mode, time) => instruction_func::sta(self, bus, mode, *time),
            Instruction::STX(mode, time) => instruction_func::stx(self, bus, mode, *time),
            Instruction::STY(mode, time) => instruction_func::sty(self, bus, mode, *time),
            Instruction::TAX(mode, time) => instruction_func::tax(self, bus, mode, *time),
            Instruction::TAY(mode, time) => instruction_func::tay(self, bus, mode, *time),
            Instruction::TSX(mode, time) => instruction_func::tsx(self, bus, mode, *time),
            Instruction::TXA(mode, time) => instruction_func::txa(self, bus, mode, *time),
            Instruction::TXS(mode, time) => instruction_func::txs(self, bus, mode, *time),
            Instruction::TYA(mode, time) => instruction_func::tya(self, bus, mode, *time),
        };

        // loop the number of cycles it took to complete the instruction
        while cycles > 0 {
            cycles -= 1;
        }
    }
}

// helper functions
impl CPU {
    /// Check the PPU status register flags and perform the necessary CPU operations. The PPU status register is zeroed when
    /// read
    ///
    /// Arguments:
    /// * `bus` (`CPUBus`) : the bus to use
    fn check_ppu_status_and_update(&mut self, bus: &mut CPUBus) {

        //let status = bus.read(PPU_STATUS_REGISTER_ADDR);

        //self.check_vblank(bus, status);

        //bus.write(PPU_STATUS_REGISTER_ADDR, 0x0);
    }

    /// Check the VBlank flag of the PPU status and perform an NMI. TODO when do we do an NMI?
    ///
    /// Arguments:
    /// * `bus` (`CPUBus`): the bus the CPU reads from
    /// * `status` (`u8`): the PPU status flag
    fn check_vblank(&mut self, bus: &mut CPUBus, status: u8) {
        if Self::is_ppu_in_vblank(status) && !self.is_ppu_already_in_vblank {
            let mut cycles = self.nmi(bus);
            self.is_ppu_already_in_vblank = true;
            while cycles > 0 {
                cycles -= 1;
            }
        } else if !Self::is_ppu_in_vblank(status) && self.is_ppu_already_in_vblank {
            self.is_ppu_already_in_vblank = false;
        }
    }

    /// Check if the PPU is in VBlank
    fn is_ppu_in_vblank(status: u8) -> bool {
        status & 0x80 == 0x80
    }
}
