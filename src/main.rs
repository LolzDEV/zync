use std::fs;

use interpreter::Interpreter;
use lexer::Lexer;

use crate::{ast::Parser, lexer::Token};

pub mod ast;
pub mod lexer;
pub mod interpreter;

fn main() {
    let source = fs::read_to_string("./test.zy").unwrap();
    
    let mut interpreter = Interpreter::new(&source).unwrap();
    println!("{:?}", interpreter.ast);
    interpreter.interpret();

}
