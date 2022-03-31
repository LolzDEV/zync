use std::fs;

use lexer::Lexer;

use crate::{ast::Parser, lexer::Token};

pub mod ast;
pub mod lexer;

fn main() {
    let source = fs::read_to_string("./test.zy").unwrap();
    let mut lexer = Lexer::new(&source);

    loop {
        let tok = lexer.next();
        if let (Token::Eof, _) = tok {
            break;
        }

        println!("Token: {:?}", tok.0)
    }

    let mut lexer = Lexer::new(&source);

    let mut parser = Parser::new(&mut lexer);

    let ast = parser.parse();

    println!("{:?}", ast);
}
