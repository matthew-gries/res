use res::core::system;

use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        println!("Please supply a ROM path");
        return;
    }
}
