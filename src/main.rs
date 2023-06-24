mod lexer;
mod token;

use crate::lexer::Lexer;
use getopts::Options;
use std::fs::File;
use std::io::{BufReader, Read};
use std::env;

struct Flags {
    dummy: bool,
}

fn print_usage(prog_name: &str, opts: Options) {
    let brief = format!("Usage: {} [options...] filename input", prog_name);
    print!("{}", opts.usage(&brief));
}

fn execute(fname: &str, input: &str, flags: Flags) {
    let file = match File::open(fname) {
        Ok(file) => file,
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };
    let mut reader = BufReader::new(file);
    let mut source = String::new();
    match reader.read_to_string(&mut source) {
        Ok(_) => (),
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    }
    let mut lexer = Lexer::new(source);
    let stream = match lexer.lexing() {
        Ok(stream) => stream,
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };
    println!("{:?}", stream);
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let prog_name = args[0].clone();
    let mut opts = Options::new();
    opts.optflag("h", "help", "Print help.");
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("{}", e);
            return;
        },
    };
    if matches.opt_present("h") {
        print_usage(&prog_name, opts);
        return;
    };
    let flags = Flags {
        dummy: true,
    };
    let free_args = matches.free;
    if let Some(filename) = free_args.get(0) {
        let input = match free_args.get(1) {
            Some(input) => input,
            None => "",
        };
        execute(filename, input, flags);
    } else {
        eprintln!("Too few argments.") 
    }
}

