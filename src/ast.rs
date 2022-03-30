use std::ops::Deref;

use crate::lexer::{Lexer, Token};

#[derive(Debug, Clone)]
pub enum Op {
    Plus,
    Minus,
    Star,
    Slash,
    Assign,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(f64, Option<Box<Expr>>),
    BinaryExpr(
        Option<Box<Expr>>,
        Option<Op>,
        Option<Box<Expr>>,
        Option<Box<Expr>>,
    ),
    Let(Option<Box<Expr>>, Option<Box<Expr>>, Option<Box<Expr>>),
    Identifier(String),
    None,
}

pub struct Ast;

impl Ast {
    pub fn parse(lexer: &mut Lexer) -> Expr {
        let mut last = Expr::None;
        let mut current = &mut Expr::None;
        let mut last_let = Expr::None;
        loop {
            match lexer.next() {
                Token::Number(n) => match current {
                    Expr::Number(_, _) => {
                        panic!("Syntax error, expected operator but number found")
                    }
                    Expr::BinaryExpr(first, _, second, _) => {
                        if let Some(_) = first {
                            if let Some(_) = second {
                                panic!("Syntax error, operator expected but number found");
                            } else {
                                *second = Some(Box::new(Expr::Number(n, None)));
                            }
                        } else {
                            *first = Some(Box::new(Expr::Number(n, None)));
                        }
                    }
                    Expr::None => *current = Expr::Number(n, None),
                    Expr::Let(identifier, expr, _) => {
                        if let Some(_) = identifier {
                            if let Some(ex) = expr {
                                let ex = ex.as_mut();
                                if let Expr::BinaryExpr(first, op, second, _) = ex {
                                    if let Some(_) = first {
                                        if let Some(_) = op {
                                            if let None = second {
                                                *second = Some(Box::new(Expr::Number(n, None)));
                                            } else {
                                                panic!("Syntax error, ; expected but number found")
                                            }
                                        } else {
                                            panic!(
                                                "Syntax error, operator expected but number found"
                                            )
                                        }
                                    } else {
                                        panic!("Syntax error, unexpected number")
                                    }
                                }
                            }
                        } else {
                            panic!("Syntax error, identifier expected but number found")
                        }
                    }
                    Expr::Identifier(_) => panic!("Syntax error, unexpected identifier"),
                },
                Token::Plus => match current {
                    Expr::Number(_, _) => {
                        *current =
                            Expr::BinaryExpr(Some(Box::new(current.clone())), Some(Op::Plus), None, None);
                    }
                    Expr::BinaryExpr(first, op, second, _) => {
                        if let Some(_) = first {
                            if let None = second {
                                if let None = op {
                                    *op = Some(Op::Plus)
                                }
                            } else {
                                *current = Expr::BinaryExpr(
                                    Some(Box::new(current.clone())),
                                    Some(Op::Plus),
                                    None,
                                    None,
                                )
                            }
                        } else {
                            println!("Syntax error, expression expected but operator found")
                        }
                    }
                    Expr::None => panic!("Syntax error, expression expected but operator found"),
                    Expr::Let(_, _, _) => {}
                    Expr::Identifier(_) => panic!("Syntax error, unexpected identifier"),
                },
                Token::Minus => match current {
                    Expr::Number(_, _) => {
                        *current =
                            Expr::BinaryExpr(Some(Box::new(current.clone())), Some(Op::Minus), None, None);
                    }
                    Expr::BinaryExpr(first, op, second, _) => {
                        if let Some(_) = first {
                            if let None = second {
                                if let None = op {
                                    *op = Some(Op::Minus)
                                }
                            } else {
                                *current = Expr::BinaryExpr(
                                    Some(Box::new(current.clone())),
                                    Some(Op::Minus),
                                    None,
                                    None,
                                )
                            }
                        } else {
                            println!("Syntax error, expression expected but operator found")
                        }
                    }
                    Expr::None => panic!("Syntax error, expression expected but operator found"),
                    Expr::Let(_, _, _) => panic!("Syntax error, ; expected but operator found"),
                    Expr::Identifier(_) => panic!("Syntax error, unexpected identifier"),
                },
                Token::Star => match current {
                    Expr::Number(_, _) => {
                        *current =
                            Expr::BinaryExpr(Some(Box::new(current.clone())), Some(Op::Star), None, None);
                    }
                    Expr::BinaryExpr(first, op, second, _) => {
                        if let Some(_) = first {
                            if let None = second {
                                if let None = op {
                                    *op = Some(Op::Star)
                                }
                            } else {
                                *current = Expr::BinaryExpr(
                                    Some(Box::new(current.clone())),
                                    Some(Op::Star),
                                    None,
                                    None,
                                )
                            }
                        } else {
                            println!("Syntax error, expression expected but operator found")
                        }
                    }
                    Expr::None => panic!("Syntax error, expression expected but operator found"),
                    Expr::Let(_, _, _) => panic!("Syntax error, ; expected but operator found"),
                    Expr::Identifier(_) => panic!("Syntax error, unexpected identifier"),
                },
                Token::Slash => match &mut current {
                    Expr::Number(_, _) => {
                        *current =
                            Expr::BinaryExpr(Some(Box::new(current.clone())), Some(Op::Slash), None, None);
                    }
                    Expr::BinaryExpr(first, op, second, _) => {
                        if let Some(_) = first {
                            if let None = second {
                                if let None = op {
                                    *op = Some(Op::Slash)
                                }
                            } else {
                                *current = Expr::BinaryExpr(
                                    Some(Box::new(current.clone())),
                                    Some(Op::Slash),
                                    None,
                                    None,
                                )
                            }
                        } else {
                            println!("Syntax error, expression expected but operator found")
                        }
                    }
                    Expr::None => panic!("Syntax error, expression expected but operator found"),
                    Expr::Let(_, _, _) => panic!("Syntax error, ; expected but operator found"),
                    Expr::Identifier(_) => panic!("Syntax error, unexpected identifier"),
                },
                Token::SemiColon => {
                    if let Expr::Let(id, expr, _) = &mut last_let {
                        if let Expr::BinaryExpr(_, _, _, _) = current.clone() {
                            *expr = Some(Box::new(current.clone()));
                            *current = last_let.clone();
                        }
                    }

                    last_let = Expr::None;

                    match &mut last {
                        Expr::Number(_, then) => *then = Some(Box::new(current.clone())),
                        Expr::BinaryExpr(_, _, _, then) => *then = Some(Box::new(current.clone())),
                        Expr::Let(_, _, then) => *then = Some(Box::new(current.clone())),
                        Expr::None => last = current.clone(),
                        Expr::Identifier(_) => panic!("Syntax error, unexpected identifier"),
                    }

                    *current = Expr::None;
                }
                Token::Eof => return last,
                Token::Identifier(name) => match current {
                    Expr::Number(_, _) => {
                        panic!("Syntax error, operator expected but identifier found")
                    }
                    Expr::BinaryExpr(_, _, _, _) => {
                        panic!("Syntax error, unknown expected but identifier found")
                    }
                    Expr::Let(id, expr, then) => {
                        last_let = Expr::Let(id.clone(), expr.clone(), then.clone());
                        if let None = id {
                            *id = Some(Box::new(Expr::Identifier(name)));
                        } else {
                            panic!("Syntax error, = expected but identifier found")
                        }
                    }
                    Expr::None => *current = Expr::Identifier(name),
                    Expr::Identifier(_) => panic!("Syntax error, unexpected identifier"),
                },
                Token::Let => {
                    if let Expr::None = *current {
                        *current = Expr::Let(None, None, None);
                    } else {
                        panic!("Syntax error, unexpected `let`");
                    }
                }
                Token::Assign => match current {
                    Expr::Number(_, _) => {
                        panic!("Syntax error, identifier expected but number found");
                    }
                    Expr::BinaryExpr(first, op, second, _) => {
                        if let Some(first) = first {
                            if let Expr::Identifier(_) = (*first).deref() {
                                if let None = second {
                                    if let None = op {
                                        *op = Some(Op::Assign)
                                    }
                                } else {
                                    panic!("Syntax error, unexpected =")
                                }
                            } else {
                                panic!("Syntax error, unexpected =")
                            }
                        } else {
                            println!("Syntax error, expression expected but operator found")
                        }
                    }
                    Expr::None => panic!("Syntax error, expression expected but operator found"),
                    Expr::Let(id, _, _) => *current = Expr::BinaryExpr(id.clone(), Some(Op::Assign), None, None),
                    Expr::Identifier(_) => panic!("Syntax error, unexpected identifier"),
                },
            }
        }
    }
}
