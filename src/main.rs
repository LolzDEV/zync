use std::{
    env, fs,
    path::Path,
    process::{exit, Command},
};

use colored::Colorize;
use inkwell::context::Context;
use lexer::Lexer;
use parser::Parser;

use crate::compiler::Compiler;

pub mod compiler;
pub mod lexer;
pub mod parser;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("Usage: zync `filename`");
        exit(-1);
    }

    for file in args.iter().skip(1) {
        let source = fs::read_to_string(file).unwrap();

        let context = Context::create();
        let module = context.create_module(file);
        let builder = context.create_builder();

        let mut lexer = Lexer::new(&source);
        let mut parser = Parser::new(&mut lexer);

        println!("{:?}\n{:?}", parser.parse(), parser.tokens);

        let mut lexer = Lexer::new(&source);
        let mut parser = Parser::new(&mut lexer);

        let mut compiler = Compiler::new(&context, &builder, &module);

        println!("{}: Compiling source code", "[INFO]".green());
        let ast = parser.parse();
        if let Ok(ast) = ast.clone() {
            compiler.compile(ast);
        }

        if let Err(e) = ast {
            println!("{}", e);
            exit(-1);
        }

        println!("{}: Generating IR file", "[INFO]".green());
        module
            .print_to_file(Path::new(&format!("{}.ll", file)))
            .unwrap();

        println!("{}: Assembling IR", "[INFO]".green());
        Command::new("llc")
            .args([
                "-filetype=obj",
                &format!("{}.ll", file),
                "-o",
                &format!("{}.o", file),
            ])
            .status()
            .unwrap();
    }

    let mut objects = Vec::new();
    for file in args.iter().skip(1) {
        objects.push(format!("{}.o", file));
    }
    println!("{}: Linking", "[INFO]".green());
    Command::new("clang")
        .args(objects.as_slice())
        .args(["-o", "a.out"])
        .status()
        .unwrap();
    for file in args.iter().skip(1) {
        println!("{}: Cleaning", "[INFO]".green());
        Command::new("rm")
            .args(["-rf", &format!("{}.ll", file), &format!("{}.o", file)])
            .status()
            .unwrap();
        println!("{}: Done", "[INFO]".green());
    }
}
