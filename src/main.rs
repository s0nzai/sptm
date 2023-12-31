mod error;
mod lexer;
mod token;
mod tree;
mod parser;
mod expand;
mod gen;
mod quintuples;
mod optimize;
mod machine;

use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::expand::Expand;
use crate::error::Result;
use crate::gen::Gen;
use crate::optimize::optimize;
use crate::machine::{Machine, Config};
use getopts::Options;
use std::fs::File;
use std::io::{BufReader, Read};
use std::env;

struct Flags {
    print_table: bool,
    result: bool,
}

fn print_usage(prog_name: &str, opts: Options) {
    let brief = format!("Usage: {} [options...] filename input", prog_name);
    print!("{}", opts.usage(&brief));
}

fn execute(source: String, input: &str, flags: Flags) -> Result<()> {
    let mut lexer = Lexer::new(source);
    let stream = lexer.lexing()?;
    let mut parser = Parser::new(stream);
    let tree = parser.parse()?;
    let mut expand = Expand::new();
    let proc = expand.expand_proc(tree, Vec::new())?;
    let mut gen = Gen::new();
    let q = gen.gen(proc);
    let q = optimize(q);
    println!("{}", q);
    if flags.print_table {
        return Ok(());
    }
    let c = Config::new(input.to_string());
    println!("{}", c);
    let mut machine = Machine::new(q, c);
    let mut j = 0;
    while let Some(c) = machine.run_step() {
        j += 1;
        if !flags.result {
            println!("{}", c);
        }
    }
    if flags.result {
        println!("{}", machine.config);
    }
    println!("steps: {}", j);
    Ok(())
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let prog_name = args[0].clone();
    let mut opts = Options::new();
    opts.optflag("h", "help", "Print help.");
    opts.optflag("p", "print-table", "Print table only.");
    opts.optflag("r", "result", "Print result only.");
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
        print_table: matches.opt_present("p"),
        result: matches.opt_present("r"),
    };
    let free_args = matches.free;
    if let Some(filename) = free_args.get(0) {
        let input = match free_args.get(1) {
            Some(input) => input,
            None => "",
        };
        let file = match File::open(filename) {
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
        match execute(source, input, flags) {
            Ok(_) => (),
            Err(e) => {
                eprintln!("{}", e);
                return;
            }
        }
    } else {
        eprintln!("Too few argments.") 
    }
}

