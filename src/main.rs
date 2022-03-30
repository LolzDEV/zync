use std::fs;

use lexer::Lexer;

use crate::{ast::Ast, lexer::Token};

pub mod lexer;
pub mod ast;

fn main() {
    let source = fs::read_to_string("./test.zy").unwrap();
    let mut lexer = Lexer::new(&source);

    loop {
        let tok = lexer.next();
        if let Token::Eof = tok {
            break;
        }

        println!("Token: {:?}", tok)
    }

    let mut lexer = Lexer::new(&source);

    let ast = Ast::parse(&mut lexer);

    println!("{:?}", ast);
}
