use crate::core::instruction::*;

use crate::core::bus::CPUBus;
use crate::core::cpu::CPU;


fn generate_cpu_and_bus() -> (CPU, CPUBus) {
    (CPU::new(), CPUBus::new())
}


#[test]
fn adc_imm_test_no_carry_no_overflow() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x69);
    bus.write(1, 0x20);
    cpu.a = 0x24;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::ADC(mode, time) = instr {
        let cycles = instruction_func::adc(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x69);
    bus.write(1, 0xFF);
    cpu.a = 0x01;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::ADC(mode, time) = instr {
        let cycles = instruction_func::adc(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x69);
    bus.write(1, 0x01);
    cpu.a = 0xFE;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::ADC(mode, time) = instr {
        let cycles = instruction_func::adc(&mut cpu, &mut bus, mode, *time); // will do 1 + -2
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x69);
    bus.write(1, 0x50);
    cpu.a = 0x50;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::ADC(mode, time) = instr {
        let cycles = instruction_func::adc(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x69);
    bus.write(1, 0x50);
    cpu.a = 0xD0;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::ADC(mode, time) = instr {
        let cycles = instruction_func::adc(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x69);
    bus.write(1, 0xD0);
    cpu.a = 0x90;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::ADC(mode, time) = instr {
        let cycles = instruction_func::adc(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x29);
    bus.write(1, 0xF0);
    cpu.a = 0x1F;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::AND(mode, time) = instr {
        let cycles = instruction_func::and(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x29);
    bus.write(1, 0xF0);
    cpu.a = 0x0F;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::AND(mode, time) = instr {
        let cycles = instruction_func::and(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x29);
    bus.write(1, 0xF0);
    cpu.a = 0x8F;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::AND(mode, time) = instr {
        let cycles = instruction_func::and(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x0A);
    cpu.a = 0x01;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::ASL(mode, time) = instr {
        let cycles = instruction_func::asl(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x0A);
    cpu.a = 0x80;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::ASL(mode, time) = instr {
        let cycles = instruction_func::asl(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x0A);
    cpu.a = 0x40;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::ASL(mode, time) = instr {
        let cycles = instruction_func::asl(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x0E);
    bus.write(1, 0x40);
    bus.write(2, 0x01);
    bus.write(0x0140, 0x01);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::ASL(mode, time) = instr {
        let cycles = instruction_func::asl(&mut cpu, &mut bus, mode, *time);
        assert_eq!(bus.read(0x0140), 0x02);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.c = true;
    bus.write(0, 0x90);
    bus.write(1, 0x20);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BCC(mode, time) = instr {
        let cycles = instruction_func::bcc(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 2);
        assert_eq!(cpu.pc, 0x2);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bcc_pos_offset_same_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x90);
    bus.write(1, 0x20);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BCC(mode, time) = instr {
        let cycles = instruction_func::bcc(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 3);
        assert_eq!(cpu.pc, 0x22);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bcc_pos_offset_diff_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.pc = 0xF0;
    bus.write(0xF0, 0x90);
    bus.write(0xF1, 0x20);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BCC(mode, time) = instr {
        let cycles = instruction_func::bcc(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 0x112);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bcc_neg_offset_same_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.pc = 0x7F01;
    bus.write(0x7F01, 0x90);
    bus.write(0x7F02, 0xFD);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BCC(mode, time) = instr {
        let cycles = instruction_func::bcc(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 3);
        assert_eq!(cpu.pc, 0x7F00);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bcc_neg_offset_diff_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.pc = 0x7F00;
    bus.write(0x7F00, 0x90);
    bus.write(0x7F01, 0xFD);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BCC(mode, time) = instr {
        let cycles = instruction_func::bcc(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 0x7EFF);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bcs_no_branch_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.c = false;
    bus.write(0, 0xB0);
    bus.write(1, 0x20);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BCS(mode, time) = instr {
        let cycles = instruction_func::bcs(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 2);
        assert_eq!(cpu.pc, 0x2);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bcs_pos_offset_same_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.c = true;
    bus.write(0, 0xB0);
    bus.write(1, 0x20);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BCS(mode, time) = instr {
        let cycles = instruction_func::bcs(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 3);
        assert_eq!(cpu.pc, 0x22);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bcs_pos_offset_diff_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.c = true;
    cpu.pc = 0xF0;
    bus.write(0xF0, 0xB0);
    bus.write(0xF1, 0x20);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BCS(mode, time) = instr {
        let cycles = instruction_func::bcs(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 0x112);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bcs_neg_offset_same_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.c = true;
    cpu.pc = 0x7F01;
    bus.write(0x7F01, 0xB0);
    bus.write(0x7F02, 0xFD);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BCS(mode, time) = instr {
        let cycles = instruction_func::bcs(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 3);
        assert_eq!(cpu.pc, 0x7F00);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bcs_neg_offset_diff_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.c = true;
    cpu.pc = 0x7F00;
    bus.write(0x7F00, 0xB0);
    bus.write(0x7F01, 0xFD);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BCS(mode, time) = instr {
        let cycles = instruction_func::bcs(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 0x7EFF);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn beq_no_branch_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.z = false;
    bus.write(0, 0xF0);
    bus.write(1, 0x20);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BEQ(mode, time) = instr {
        let cycles = instruction_func::beq(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 2);
        assert_eq!(cpu.pc, 0x2);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn beq_pos_offset_same_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.z = true;
    bus.write(0, 0xF0);
    bus.write(1, 0x20);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BEQ(mode, time) = instr {
        let cycles = instruction_func::beq(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 3);
        assert_eq!(cpu.pc, 0x22);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn beq_pos_offset_diff_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.z = true;
    cpu.pc = 0xF0;
    bus.write(0xF0, 0xF0);
    bus.write(0xF1, 0x20);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BEQ(mode, time) = instr {
        let cycles = instruction_func::beq(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 0x112);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn beq_neg_offset_same_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.z = true;
    cpu.pc = 0x7F01;
    bus.write(0x7F01, 0xF0);
    bus.write(0x7F02, 0xFD);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BEQ(mode, time) = instr {
        let cycles = instruction_func::beq(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 3);
        assert_eq!(cpu.pc, 0x7F00);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn beq_neg_offset_diff_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.z = true;
    cpu.pc = 0x7F00;
    bus.write(0x7F00, 0xF0);
    bus.write(0x7F01, 0xFD);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BEQ(mode, time) = instr {
        let cycles = instruction_func::beq(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 0x7EFF);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bne_no_branch_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.z = true;
    bus.write(0, 0xD0);
    bus.write(1, 0x20);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BNE(mode, time) = instr {
        let cycles = instruction_func::bne(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 2);
        assert_eq!(cpu.pc, 0x2);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bne_pos_offset_same_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.z = false;
    bus.write(0, 0xD0);
    bus.write(1, 0x20);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BNE(mode, time) = instr {
        let cycles = instruction_func::bne(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 3);
        assert_eq!(cpu.pc, 0x22);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bne_pos_offset_diff_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.z = false;
    cpu.pc = 0xF0;
    bus.write(0xF0, 0xD0);
    bus.write(0xF1, 0x20);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BNE(mode, time) = instr {
        let cycles = instruction_func::bne(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 0x112);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bne_neg_offset_same_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.z = false;
    cpu.pc = 0x7F01;
    bus.write(0x7F01, 0xD0);
    bus.write(0x7F02, 0xFD);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BNE(mode, time) = instr {
        let cycles = instruction_func::bne(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 3);
        assert_eq!(cpu.pc, 0x7F00);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bne_neg_offset_diff_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.z = false;
    cpu.pc = 0x7F00;
    bus.write(0x7F00, 0xD0);
    bus.write(0x7F01, 0xFD);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BNE(mode, time) = instr {
        let cycles = instruction_func::bne(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 0x7EFF);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bmi_no_branch_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.n = false;
    bus.write(0, 0x30);
    bus.write(1, 0x20);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BMI(mode, time) = instr {
        let cycles = instruction_func::bmi(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 2);
        assert_eq!(cpu.pc, 0x2);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bmi_pos_offset_same_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.n = true;
    bus.write(0, 0x30);
    bus.write(1, 0x20);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BMI(mode, time) = instr {
        let cycles = instruction_func::bmi(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 3);
        assert_eq!(cpu.pc, 0x22);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bmi_pos_offset_diff_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.n = true;
    cpu.pc = 0xF0;
    bus.write(0xF0, 0x30);
    bus.write(0xF1, 0x20);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BMI(mode, time) = instr {
        let cycles = instruction_func::bmi(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 0x112);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bmi_neg_offset_same_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.n = true;
    cpu.pc = 0x7F01;
    bus.write(0x7F01, 0x30);
    bus.write(0x7F02, 0xFD);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BMI(mode, time) = instr {
        let cycles = instruction_func::bmi(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 3);
        assert_eq!(cpu.pc, 0x7F00);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bmi_neg_offset_diff_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.n = true;
    cpu.pc = 0x7F00;
    bus.write(0x7F00, 0x30);
    bus.write(0x7F01, 0xFD);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BMI(mode, time) = instr {
        let cycles = instruction_func::bmi(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 0x7EFF);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bpl_no_branch_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.n = true;
    bus.write(0, 0x10);
    bus.write(1, 0x20);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BPL(mode, time) = instr {
        let cycles = instruction_func::bpl(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 2);
        assert_eq!(cpu.pc, 0x2);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bpl_pos_offset_same_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.n = false;
    bus.write(0, 0x10);
    bus.write(1, 0x20);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BPL(mode, time) = instr {
        let cycles = instruction_func::bpl(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 3);
        assert_eq!(cpu.pc, 0x22);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bpl_pos_offset_diff_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.n = false;
    cpu.pc = 0xF0;
    bus.write(0xF0, 0x10);
    bus.write(0xF1, 0x20);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BPL(mode, time) = instr {
        let cycles = instruction_func::bpl(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 0x112);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bpl_neg_offset_same_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.n = false;
    cpu.pc = 0x7F01;
    bus.write(0x7F01, 0x10);
    bus.write(0x7F02, 0xFD);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BPL(mode, time) = instr {
        let cycles = instruction_func::bpl(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 3);
        assert_eq!(cpu.pc, 0x7F00);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bpl_neg_offset_diff_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.n = false;
    cpu.pc = 0x7F00;
    bus.write(0x7F00, 0x10);
    bus.write(0x7F01, 0xFD);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BPL(mode, time) = instr {
        let cycles = instruction_func::bpl(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 0x7EFF);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bvc_no_branch_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.v = true;
    bus.write(0, 0x50);
    bus.write(1, 0x20);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BVC(mode, time) = instr {
        let cycles = instruction_func::bvc(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 2);
        assert_eq!(cpu.pc, 0x2);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bvc_pos_offset_same_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.v = false;
    bus.write(0, 0x50);
    bus.write(1, 0x20);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BVC(mode, time) = instr {
        let cycles = instruction_func::bvc(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 3);
        assert_eq!(cpu.pc, 0x22);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bvc_pos_offset_diff_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.v = false;
    cpu.pc = 0xF0;
    bus.write(0xF0, 0x50);
    bus.write(0xF1, 0x20);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BVC(mode, time) = instr {
        let cycles = instruction_func::bvc(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 0x112);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bvc_neg_offset_same_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.v = false;
    cpu.pc = 0x7F01;
    bus.write(0x7F01, 0x50);
    bus.write(0x7F02, 0xFD);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BVC(mode, time) = instr {
        let cycles = instruction_func::bvc(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 3);
        assert_eq!(cpu.pc, 0x7F00);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bvc_neg_offset_diff_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.v = false;
    cpu.pc = 0x7F00;
    bus.write(0x7F00, 0x50);
    bus.write(0x7F01, 0xFD);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BVC(mode, time) = instr {
        let cycles = instruction_func::bvc(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 0x7EFF);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bvs_no_branch_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.v = false;
    bus.write(0, 0x70);
    bus.write(1, 0x20);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BVS(mode, time) = instr {
        let cycles = instruction_func::bvs(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 2);
        assert_eq!(cpu.pc, 0x2);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bvs_pos_offset_same_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.v = true;
    bus.write(0, 0x70);
    bus.write(1, 0x20);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BVS(mode, time) = instr {
        let cycles = instruction_func::bvs(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 3);
        assert_eq!(cpu.pc, 0x22);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bvs_pos_offset_diff_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.v = true;
    cpu.pc = 0xF0;
    bus.write(0xF0, 0x70);
    bus.write(0xF1, 0x20);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BVS(mode, time) = instr {
        let cycles = instruction_func::bvs(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 0x112);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bvs_neg_offset_same_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.v = true;
    cpu.pc = 0x7F01;
    bus.write(0x7F01, 0x70);
    bus.write(0x7F02, 0xFD);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BVS(mode, time) = instr {
        let cycles = instruction_func::bvs(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 3);
        assert_eq!(cpu.pc, 0x7F00);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bvs_neg_offset_diff_page_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.p.v = true;
    cpu.pc = 0x7F00;
    bus.write(0x7F00, 0x70);
    bus.write(0x7F01, 0xFD);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BVS(mode, time) = instr {
        let cycles = instruction_func::bvs(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 4);
        assert_eq!(cpu.pc, 0x7EFF);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn bit_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x24);
    bus.write(1, 0x40);
    bus.write(0x0040, 0xE1);
    cpu.a = 2;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BIT(mode, time) = instr {
        let cycles = instruction_func::bit(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.pc = 0x1234; // M[0x1234] = 0 => Break
    cpu.sp = 0xFF;
    cpu.p.n = true;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::BRK(mode, time) = instr {
        let cycles = instruction_func::brk(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 7);
        assert_eq!(cpu.p.b, true);
        assert_eq!(cpu.pc, 0x0000);
        assert_eq!(bus.read(0x01FF), 0x35);
        assert_eq!(bus.read(0x01FE), 0x12);
        assert_eq!(bus.read(0x01FD), 0x94);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
    
    
}


#[test]
fn cmp_imm_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xC9);
    bus.write(1, 0x02);
    cpu.a = 0x03;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::CMP(mode, time) = instr {
        let cycles = instruction_func::cmp(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xC9);
    bus.write(1, 0x03);
    cpu.a = 0x04;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::CMP(mode, time) = instr {
        let cycles = instruction_func::cmp(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xC9);
    bus.write(1, 0x03);
    cpu.a = 0x03;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::CMP(mode, time) = instr {
        let cycles = instruction_func::cmp(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xC9);
    bus.write(1, 0x03);
    cpu.a = 0x02;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::CMP(mode, time) = instr {
        let cycles = instruction_func::cmp(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xE0);
    bus.write(1, 0x02);
    cpu.x = 0x03;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::CPX(mode, time) = instr {
        let cycles = instruction_func::cpx(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xE0);
    bus.write(1, 0x03);
    cpu.x = 0x04;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::CPX(mode, time) = instr {
        let cycles = instruction_func::cpx(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xE0);
    bus.write(1, 0x03);
    cpu.x = 0x03;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::CPX(mode, time) = instr {
        let cycles = instruction_func::cpx(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xE0);
    bus.write(1, 0x03);
    cpu.x = 0x02;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::CPX(mode, time) = instr {
        let cycles = instruction_func::cpx(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xC0);
    bus.write(1, 0x02);
    cpu.y = 0x03;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::CPY(mode, time) = instr {
        let cycles = instruction_func::cpy(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xC0);
    bus.write(1, 0x03);
    cpu.y = 0x04;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::CPY(mode, time) = instr {
        let cycles = instruction_func::cpy(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xC0);
    bus.write(1, 0x03);
    cpu.y = 0x03;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::CPY(mode, time) = instr {
        let cycles = instruction_func::cpy(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xC0);
    bus.write(1, 0x03);
    cpu.y = 0x02;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::CPY(mode, time) = instr {
        let cycles = instruction_func::cpy(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x00FF, 2);
    bus.write(0, 0xC6);
    bus.write(1, 0xFF);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::DEC(mode, time) = instr {
        let cycles = instruction_func::dec(&mut cpu, &mut bus, mode, *time);
        assert_eq!(bus.read(0x00FF), 1);
        assert_eq!(cycles, 5);
        assert_eq!(cpu.p.z, false);
        assert_eq!(cpu.p.n, false);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn dec_zp_zero_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x00FF, 1);
    bus.write(0, 0xC6);
    bus.write(1, 0xFF);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::DEC(mode, time) = instr {
        let cycles = instruction_func::dec(&mut cpu, &mut bus, mode, *time);
        assert_eq!(bus.read(0x00FF), 0);
        assert_eq!(cycles, 5);
        assert_eq!(cpu.p.z, true);
        assert_eq!(cpu.p.n, false);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn dec_zp_neg_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x00FF, 0x81);
    bus.write(0, 0xC6);
    bus.write(1, 0xFF);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::DEC(mode, time) = instr {
        let cycles = instruction_func::dec(&mut cpu, &mut bus, mode, *time);
        assert_eq!(bus.read(0x00FF), 0x80);
        assert_eq!(cycles, 5);
        assert_eq!(cpu.p.z, false);
        assert_eq!(cpu.p.n, true);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn dex_zp_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xCA);
    cpu.x = 2;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::DEX(mode, time) = instr {
        let cycles = instruction_func::dex(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xCA);
    cpu.x = 0x01;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::DEX(mode, time) = instr {
        let cycles = instruction_func::dex(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xCA);
    cpu.x = 0x81;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::DEX(mode, time) = instr {
        let cycles = instruction_func::dex(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x88);
    cpu.y = 2;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::DEY(mode, time) = instr {
        let cycles = instruction_func::dey(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x88);
    cpu.y = 0x01;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::DEY(mode, time) = instr {
        let cycles = instruction_func::dey(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x88);
    cpu.y = 0x81;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::DEY(mode, time) = instr {
        let cycles = instruction_func::dey(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x49);
    bus.write(1, 0xF0);
    cpu.a = 0x8F;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::EOR(mode, time) = instr {
        let cycles = instruction_func::eor(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x49);
    bus.write(1, 0xFF);
    cpu.a = 0xFF;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::EOR(mode, time) = instr {
        let cycles = instruction_func::eor(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x49);
    bus.write(1, 0xF0);
    cpu.a = 0x0F;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::EOR(mode, time) = instr {
        let cycles = instruction_func::eor(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x00FF, 1);
    bus.write(0, 0xE6);
    bus.write(1, 0xFF);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::INC(mode, time) = instr {
        let cycles = instruction_func::inc(&mut cpu, &mut bus, mode, *time);
        assert_eq!(bus.read(0x00FF), 2);
        assert_eq!(cycles, 5);
        assert_eq!(cpu.p.z, false);
        assert_eq!(cpu.p.n, false);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn inc_zp_zero_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x00FF, 0xFF);
    bus.write(0, 0xE6);
    bus.write(1, 0xFF);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::INC(mode, time) = instr {
        let cycles = instruction_func::inc(&mut cpu, &mut bus, mode, *time);
        assert_eq!(bus.read(0x00FF), 0);
        assert_eq!(cycles, 5);
        assert_eq!(cpu.p.z, true);
        assert_eq!(cpu.p.n, false);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn inc_zp_neg_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x00FF, 0x7F);
    bus.write(0, 0xE6);
    bus.write(1, 0xFF);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::INC(mode, time) = instr {
        let cycles = instruction_func::inc(&mut cpu, &mut bus, mode, *time);
        assert_eq!(bus.read(0x00FF), 0x80);
        assert_eq!(cycles, 5);
        assert_eq!(cpu.p.z, false);
        assert_eq!(cpu.p.n, true);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn inx_zp_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xE8);
    cpu.x = 1;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::INX(mode, time) = instr {
        let cycles = instruction_func::inx(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xE8);
    cpu.x = 0xFF;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::INX(mode, time) = instr {
        let cycles = instruction_func::inx(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xE8);
    cpu.x = 0x7F;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::INX(mode, time) = instr {
        let cycles = instruction_func::inx(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xC8);
    cpu.y = 1;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::INY(mode, time) = instr {
        let cycles = instruction_func::iny(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x4C);
    bus.write(1, 0x40);
    bus.write(2, 0x01);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::JMP(mode, time) = instr {
        let cycles = instruction_func::jmp(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cpu.pc, 0x0140);
        assert_eq!(cycles, 3);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn jmp_ind_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x03FE, 0x40);
    bus.write(0x03FF, 0x01);
    bus.write(0, 0x6C);
    bus.write(1, 0xFE);
    bus.write(2, 0x03);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::JMP(mode, time) = instr {
        let cycles = instruction_func::jmp(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cpu.pc, 0x0140);
        assert_eq!(cycles, 5);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn iny_zp_zero_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xC8);
    cpu.y = 0xFF;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::INY(mode, time) = instr {
        let cycles = instruction_func::iny(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xC8);
    cpu.y = 0x7F;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::INY(mode, time) = instr {
        let cycles = instruction_func::iny(&mut cpu, &mut bus, mode, *time);
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
    
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x0240, 0x20);
    bus.write(0x0241, 0x40);
    bus.write(0x0242, 0x01);
    cpu.pc = 0x0240;
    cpu.sp = 0xFF;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::JSR(mode, time) = instr {
        let cycles = instruction_func::jsr(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cpu.pc, 0x0140);
        assert_eq!(bus.read(0x01FF), 0x42);
        assert_eq!(bus.read(0x01FE), 0x02);
        assert_eq!(cycles, 6);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn ldx_imm_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xA2);
    bus.write(1, 0x44);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LDX(mode, time) = instr {
        let cycles = instruction_func::ldx(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x0040, 0x44);  // M[0x0040] <- 0x4
    bus.write(0, 0xA6);
    bus.write(1, 0x40);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LDX(mode, time) = instr {
        let cycles = instruction_func::ldx(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x0040, 0x44);  // M[0x0040] <- 0x4
    bus.write(0, 0xB6);
    bus.write(1, 0x3F);
    cpu.y = 1;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LDX(mode, time) = instr {
        let cycles = instruction_func::ldx(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x0140, 0x44);
    bus.write(0, 0xAE);
    bus.write(1, 0x40);
    bus.write(2, 0x01);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LDX(mode, time) = instr {
        let cycles = instruction_func::ldx(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x0140, 0x44);
    bus.write(0, 0xBE);
    bus.write(1, 0x3F);
    bus.write(2, 0x01);
    cpu.y = 0x01;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LDX(mode, time) = instr {
        let cycles = instruction_func::ldx(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x0140, 0x44);
    bus.write(0, 0xBE);
    bus.write(1, 0xC1);
    bus.write(2, 0x00);
    cpu.y = 0x7F;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LDX(mode, time) = instr {
        let cycles = instruction_func::ldx(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xA0);
    bus.write(1, 0x44);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LDY(mode, time) = instr {
        let cycles = instruction_func::ldy(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x0040, 0x44);  // M[0x0040] <- 0x4
    bus.write(0, 0xA4);
    bus.write(1, 0x40);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LDY(mode, time) = instr {
        let cycles = instruction_func::ldy(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x0040, 0x44);  // M[0x0040] <- 0x4;
    bus.write(0, 0xB4);
    bus.write(1, 0x3F);
    cpu.x = 1;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LDY(mode, time) = instr {
        let cycles = instruction_func::ldy(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x0140, 0x44);
    bus.write(0, 0xAC);
    bus.write(1, 0x40);
    bus.write(2, 0x01);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LDY(mode, time) = instr {
        let cycles = instruction_func::ldy(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x0140, 0x44);
    bus.write(0, 0xBC);
    bus.write(1, 0x3F);
    bus.write(2, 0x01);
    cpu.x = 0x01;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LDY(mode, time) = instr {
        let cycles = instruction_func::ldy(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x0140, 0x44);
    bus.write(0, 0xBC);
    bus.write(1, 0xC1);
    bus.write(2, 0x00);
    cpu.x = 0x7F;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LDY(mode, time) = instr {
        let cycles = instruction_func::ldy(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xA9);
    bus.write(1, 0x44);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LDA(mode, time) = instr {
        let cycles = instruction_func::lda(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x0040, 0x44);  // M[0x0040] <- 0x4
    bus.write(0, 0xA5); // LDA 
    bus.write(1, 0x40);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LDA(mode, time) = instr {
        let cycles = instruction_func::lda(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x0040, 0x44);  // M[0x0040] <- 0x4
    bus.write(0, 0xB5);
    bus.write(1, 0x3F);
    cpu.x = 1;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LDA(mode, time) = instr {
        let cycles = instruction_func::lda(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x0140, 0x44);
    bus.write(0, 0xAD);
    bus.write(1, 0x40);
    bus.write(2, 0x01);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LDA(mode, time) = instr {
        let cycles = instruction_func::lda(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x0140, 0x44);
    bus.write(0, 0xBD);
    bus.write(1, 0x3F);
    bus.write(2, 0x01);
    cpu.x = 0x01;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LDA(mode, time) = instr {
        let cycles = instruction_func::lda(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x0140, 0x44);
    bus.write(0, 0xBD);
    bus.write(1, 0xC1);
    bus.write(2, 0x00);
    cpu.x = 0x7F;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LDA(mode, time) = instr {
        let cycles = instruction_func::lda(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x0140, 0x44);
    bus.write(0, 0xB9);
    bus.write(1, 0x3F);
    bus.write(2, 0x01);
    cpu.y = 0x01;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LDA(mode, time) = instr {
        let cycles = instruction_func::lda(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x0140, 0x44);
    bus.write(0, 0xB9);
    bus.write(1, 0xC1);
    bus.write(2, 0x00);
    cpu.y = 0x7F;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LDA(mode, time) = instr {
        let cycles = instruction_func::lda(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x0140, 0x44);
    bus.write(0, 0xA1);
    bus.write(1, 0xFD);
    bus.write(0xFE, 0x40);
    bus.write(0xFF, 0x01);
    cpu.x = 1;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LDA(mode, time) = instr {
        let cycles = instruction_func::lda(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x0140, 0x44);
    bus.write(0, 0xB1);
    bus.write(1, 0xFE);
    bus.write(0xFE, 0x3F);
    bus.write(0xFF, 0x01);
    cpu.y = 1;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LDA(mode, time) = instr {
        let cycles = instruction_func::lda(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0x0140, 0x44);
    bus.write(0, 0xB1);
    bus.write(1, 0xFE);
    bus.write(0xFE, 0xC1);
    bus.write(0xFF, 0x00);
    cpu.y = 0x7F;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LDA(mode, time) = instr {
        let cycles = instruction_func::lda(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xA9);
    bus.write(1, 0x00);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LDA(mode, time) = instr {
        let cycles = instruction_func::lda(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xA9);
    bus.write(1, 0x80);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LDA(mode, time) = instr {
        let cycles = instruction_func::lda(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x4A);
    cpu.a = 0x02;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LSR(mode, time) = instr {
        let cycles = instruction_func::lsr(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x4A);
    cpu.a = 0x01;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LSR(mode, time) = instr {
        let cycles = instruction_func::lsr(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x4E);
    bus.write(1, 0x40);
    bus.write(2, 0x01);
    bus.write(0x0140, 0x02);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::LSR(mode, time) = instr {
        let cycles = instruction_func::lsr(&mut cpu, &mut bus, mode, *time);
        assert_eq!(bus.read(0x0140), 0x01);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x09);
    bus.write(1, 0x7F);
    cpu.a = 0x00;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::ORA(mode, time) = instr {
        let cycles = instruction_func::ora(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x09);
    bus.write(1, 0x00);
    cpu.a = 0x00;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::ORA(mode, time) = instr {
        let cycles = instruction_func::ora(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x09);
    bus.write(1, 0x0F);
    cpu.a = 0xF0;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::ORA(mode, time) = instr {
        let cycles = instruction_func::ora(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x2A);
    cpu.a = 0x01;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::ROL(mode, time) = instr {
        let cycles = instruction_func::rol(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x2A);
    cpu.a = 0x80;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::ROL(mode, time) = instr {
        let cycles = instruction_func::rol(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x2A);
    cpu.a = 0x40;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::ROL(mode, time) = instr {
        let cycles = instruction_func::rol(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x2A);
    cpu.p.c = true;
    cpu.a = 0x01;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::ROL(mode, time) = instr {
        let cycles = instruction_func::rol(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x2E);
    bus.write(1, 0x40);
    bus.write(2, 0x01);
    bus.write(0x0140, 0x01);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::ROL(mode, time) = instr {
        let cycles = instruction_func::rol(&mut cpu, &mut bus, mode, *time);
        assert_eq!(bus.read(0x0140), 0x02);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x6A);
    cpu.a = 0x02;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::ROR(mode, time) = instr {
        let cycles = instruction_func::ror(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x6A);
    cpu.a = 0x02;
    cpu.p.c = true;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::ROR(mode, time) = instr {
        let cycles = instruction_func::ror(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x6A);
    cpu.a = 0x01;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::ROR(mode, time) = instr {
        let cycles = instruction_func::ror(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x6E);
    bus.write(1, 0x40);
    bus.write(2, 0x01);
    bus.write(0x0140, 0x02);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::ROR(mode, time) = instr {
        let cycles = instruction_func::ror(&mut cpu, &mut bus, mode, *time);
        assert_eq!(bus.read(0x0140), 0x01);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.sp = 0xFF;
    cpu.a = 0xBE;
    bus.write(0, 0x48);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::PHA(mode, time) = instr {
        let cycles = instruction_func::pha(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 3);
        assert_eq!(bus.read(0x01FF), 0xBE);
        assert_eq!(cpu.sp, 0xFE);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn php_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.sp = 0xFF;
    cpu.p.n = true;
    cpu.p.c = true;
    bus.write(0, 0x08);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::PHP(mode, time) = instr {
        let cycles = instruction_func::php(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 3);
        assert_eq!(bus.read(0x01FF), 0x81);
        assert_eq!(cpu.sp, 0xFE);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn pla_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.sp = 0xFE;
    bus.write(0x01FF, 0x7F);
    bus.write(0, 0x68);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::PLA(mode, time) = instr {
        let cycles = instruction_func::pla(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.sp = 0xFE;
    cpu.a = 0xFF;
    bus.write(0x01FF, 0x00);
    bus.write(0, 0x68);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::PLA(mode, time) = instr {
        let cycles = instruction_func::pla(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.sp = 0xFE;
    bus.write(0x01FF, 0xFF);
    bus.write(0, 0x68);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::PLA(mode, time) = instr {
        let cycles = instruction_func::pla(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    cpu.sp = 0xFE;
    bus.write(0x01FF, 0xFF);
    bus.write(0, 0x28);
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::PLP(mode, time) = instr {
        let cycles = instruction_func::plp(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x60);
    bus.write(0x01FF, 0x42);
    bus.write(0x01FE, 0x02);
    cpu.sp = 0xFD;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::RTS(mode, time) = instr {
        let cycles = instruction_func::rts(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cycles, 6);
        assert_eq!(cpu.pc, 0x0243);
        assert_eq!(cpu.sp, 0xFF);
    } else {
        panic!("Wrong instruction, got {:?}", instr);
    }
}


#[test]
fn rti_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x40);
    bus.write(0x01FF, 0x43);
    bus.write(0x01FE, 0x02);
    bus.write(0x01FD, 0xFF);
    cpu.sp = 0xFC;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::RTI(mode, time) = instr {
        let cycles = instruction_func::rti(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xA9); // LDA #$4
    bus.write(1, 0x44);
    bus.write(2, 0x85); // STA $4
    bus.write(3, 0x40);
    cpu.instruction_cycle(&mut bus); // execte the LDA instruction
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::STA(mode, time) = instr {
        let cycles = instruction_func::sta(&mut cpu, &mut bus, mode, *time);
        assert_eq!(cpu.a, 0x44);
        assert_eq!(bus.read(0x0040), 0x44);
        assert_eq!(cycles, 3);
        assert_eq!(cpu.p.z, false);
        assert_eq!(cpu.p.n, false);
    }
}


#[test]
fn stx_zp_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x86); // STX $4
    bus.write(1, 0x40);
    cpu.x = 0x44;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::STX(mode, time) = instr {
        let cycles = instruction_func::stx(&mut cpu, &mut bus, mode, *time);
        assert_eq!(bus.read(0x0040), 0x44);
        assert_eq!(cycles, 3);
    }
}


#[test]
fn sty_zp_test() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0x84); // STY $4
    bus.write(1, 0x40);
    cpu.y = 0x44;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::STY(mode, time) = instr {
        let cycles = instruction_func::sty(&mut cpu, &mut bus, mode, *time);
        assert_eq!(bus.read(0x0040), 0x44);
        assert_eq!(cycles, 3);
    }
}


#[test]
fn sbc_imm_test_no_carry_no_overflow() {
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xE9);
    bus.write(1, 0x30);
    cpu.a = 0x50;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::SBC(mode, time) = instr {
        let cycles = instruction_func::sbc(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xE9);
    bus.write(1, 0x01);
    cpu.a = 0x02;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::SBC(mode, time) = instr {
        let cycles = instruction_func::sbc(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xE9);
    bus.write(1, 0x30);
    cpu.a = 0xD0;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::SBC(mode, time) = instr {
        let cycles = instruction_func::sbc(&mut cpu, &mut bus, mode, *time); // will do 1 + -2
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xE9);
    bus.write(1, 0x70);
    cpu.a = 0xD0;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::SBC(mode, time) = instr {
        let cycles = instruction_func::sbc(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xE9);
    bus.write(1, 0xF0);
    cpu.a = 0xD0;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::SBC(mode, time) = instr {
        let cycles = instruction_func::sbc(&mut cpu, &mut bus, mode, *time);
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
    let (mut cpu, mut bus) = generate_cpu_and_bus();
    bus.write(0, 0xE9);
    bus.write(1, 0xB0);
    cpu.a = 0x50;
    let instr = INSTRUCTION_TABLE.get(&cpu.read_byte_and_increment(&mut bus)).unwrap();
    if let Instruction::SBC(mode, time) = instr {
        let cycles = instruction_func::sbc(&mut cpu, &mut bus, mode, *time);
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