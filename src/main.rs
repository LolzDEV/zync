use std::{env, fs, process::exit};

use ast::Parser;
use compiler::Compiler;
use inkwell::context::Context;
use lexer::Lexer;

pub mod ast;
pub mod compiler;
pub mod lexer;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("Usage: zync `filename`");
        exit(-1);
    }

    let source = fs::read_to_string(args.get(1).unwrap()).unwrap();

    let context = Context::create();
    let module = context.create_module(args.get(1).unwrap());
    let builder = context.create_builder();

    let mut lexer = Lexer::new(&source);
    let mut parser = Parser::new(&mut lexer);

    println!("{:?}\n{:?}", parser.parse(), parser.tokens);
    
    let mut lexer = Lexer::new(&source);
    let mut parser = Parser::new(&mut lexer);

    let mut compiler = Compiler::new(&context, &builder, &module);

    compiler.compile(parser.parse().unwrap());
}
