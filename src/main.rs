use res::core::system;

use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        println!("Please supply a ROM path");
        return;
    }

    let mut system = system::NesSystem::load_rom(&args[1]).unwrap();
    system.reset();
    system.run();
}
