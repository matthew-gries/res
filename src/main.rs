#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;

mod cpu;
mod memory;
mod instruction;
mod system;

use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

const BUFFER_SIZE: usize = 524288;

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() != 2 {
        println!("Please supplt ROM path.");
        return;
    }

    let rom_path = Path::new(&args[1]);
    let mut file = File::create(rom_path).unwrap();

    let mut buf: [u8; BUFFER_SIZE+16] = [0; BUFFER_SIZE+16];

    file.read(&mut buf).unwrap();

    let mut mem = memory::Memory::new();
    // load rom into program memory
    for i in 0..BUFFER_SIZE {
        mem.write((i + 0x8000) as u16, buf[16+i]);
    }

    let mut cpu = cpu::CPU::new();
    cpu.init();
    let mut system = system::NesSystem::new(cpu, mem);

    system.start();
}
