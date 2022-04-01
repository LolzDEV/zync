use std::{cell::RefCell, ops::Deref, rc::Rc};

use crate::{
    ast::{AstNode, Error, Parser},
    lexer::{Lexer, Token},
};

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub content: VariableType,
}

#[derive(Debug, Clone)]
pub enum VariableType {
    Number(f64),
    Function {
        locals: Vec<Variable>,
        parameters: Vec<AstNode>,
        block: Vec<AstNode>,
    },
    None,
}

pub struct Interpreter {
    pub ast: Vec<AstNode>,
    pub globals: Rc<RefCell<Vec<Variable>>>,
}

impl Interpreter {
    pub fn new(source: &str) -> Result<Self, Error> {
        let mut lexer = Lexer::new(source);
        let mut parser = Parser::new(&mut lexer);
        Ok(Self {
            ast: parser.parse()?,
            globals: Rc::new(RefCell::new(Vec::new())),
        })
    }

    pub fn evaluate_expr(&self, expr: AstNode, locals: Rc<RefCell<Vec<Variable>>>) -> AstNode {
        match expr {
            AstNode::Number(_) => expr,
            AstNode::BinaryExpr { lhs, op, rhs } => {
                match op {
                    crate::lexer::Token::Plus => {
                        let lhs = self.evaluate_expr(*lhs, locals.clone());

                        if let AstNode::Number(n) = lhs {
                            let rhs = self.evaluate_expr(*rhs, locals.clone());

                            if let AstNode::Number(n1) = rhs {
                                return AstNode::Number(n + n1);
                            }
                        }

                        return AstNode::None;
                    }
                    crate::lexer::Token::Minus => {
                        let lhs = self.evaluate_expr(*lhs, locals.clone());

                        if let AstNode::Number(n) = lhs {
                            let rhs = self.evaluate_expr(*rhs, locals.clone());

                            if let AstNode::Number(n1) = rhs {
                                return AstNode::Number(n - n1);
                            }
                        }

                        return AstNode::None;
                    }
                    crate::lexer::Token::Star => {
                        let lhs = self.evaluate_expr(*lhs, locals.clone());

                        if let AstNode::Number(n) = lhs {
                            let rhs = self.evaluate_expr(*rhs, locals.clone());

                            if let AstNode::Number(n1) = rhs {
                                return AstNode::Number(n * n1);
                            }
                        }

                        return AstNode::None;
                    }
                    crate::lexer::Token::Slash => {
                        let lhs = self.evaluate_expr(*lhs, locals.clone());

                        if let AstNode::Number(n) = lhs {
                            let rhs = self.evaluate_expr(*rhs, locals.clone());

                            if let AstNode::Number(n1) = rhs {
                                return AstNode::Number(n / n1);
                            }
                        }

                        return AstNode::None;
                    }
                    crate::lexer::Token::Assign => {
                        if let AstNode::Identifier(id) = *lhs.clone() {
                            let rhs = self.evaluate_expr(*rhs, locals.clone());
                            match rhs {
                                AstNode::Number(n) => {
                                    self.set_var(locals.clone(), id, VariableType::Number(n))
                                }
                                _ => (),
                            }
                        }

                        AstNode::None
                    }
                    _ => AstNode::None,
                }
            }
            AstNode::Identifier(id) => {
                let cont = self.get_var(locals.clone().borrow().deref(), id);

                match cont.content {
                    VariableType::Number(n) => AstNode::Number(n),
                    VariableType::Function {
                        locals,
                        parameters,
                        block,
                    } => todo!(),
                    VariableType::None => AstNode::None,
                }
            }
            AstNode::UnaryExpr { op, rhs } => todo!(),
            AstNode::Grouping(expr) => self.evaluate_expr(*expr, locals),
            _ => AstNode::None,
        }
    }

    pub fn get_var(&self, locals: &Vec<Variable>, id: String) -> Variable {
        for var in locals {
            if var.name == id {
                return var.clone();
            }
        }

        Variable {
            name: id,
            content: VariableType::None,
        }
    }

    pub fn set_var(&self, locals: Rc<RefCell<Vec<Variable>>>, id: String, value: VariableType) {
        for var in locals.clone().borrow_mut().iter_mut() {
            if var.name == id {
                var.content = value;
                break;
            }
        }
    }

    pub fn interpret(&mut self) {
        for statement in self.ast.iter() {
            match statement.clone() {
                AstNode::Call { id, args } => {
                    if let AstNode::Identifier(id) = *id {
                        if let AstNode::Arguments(args) = *args {
                            let mut vars = Vec::new();
                            if let Variable {
                                content: VariableType::Function { parameters, .. },
                                ..
                            } = self.get_var(self.globals.clone().borrow().deref(), id)
                            {
                            }

                            for (index, arg) in args.iter().enumerate() {
                                match self.evaluate_expr(arg.clone(), self.globals.clone()) {
                                    AstNode::Number(n) => vars.push(Variable {
                                        name: todo!(),
                                        content: todo!(),
                                    }),
                                    _ => (),
                                }
                            }
                            //self.call_function(id, vars);
                        }
                    }
                }
                AstNode::Definition {
                    id,
                    parameters,
                    block,
                } => {
                    if let AstNode::Identifier(id) = *id {
                        if let AstNode::Parameters(params) = *parameters {
                            if let AstNode::Block(block) = *block {
                                self.globals.clone().borrow_mut().push(Variable {
                                    name: id,
                                    content: VariableType::Function {
                                        locals: Vec::new(),
                                        parameters: params,
                                        block: block,
                                    },
                                });
                            }
                        }
                    }
                }
                AstNode::ExprStatement(expr) => {
                    println!(
                        "{:?}",
                        self.evaluate_expr(*expr.clone(), self.globals.clone())
                    )
                }
                AstNode::LetStatement(expr) => {
                    if let AstNode::BinaryExpr {
                        lhs,
                        op: Token::Assign,
                        ..
                    } = *expr.clone()
                    {
                        if let AstNode::Identifier(id) = *lhs.clone() {
                            self.globals.clone().borrow_mut().push(Variable {
                                name: id,
                                content: VariableType::None,
                            });
                            self.evaluate_expr(*expr.clone(), self.globals.clone());
                        }
                    }
                }
                _ => (),
            }
        }
    }

    pub fn call_function(&mut self, id: String, args: Vec<Variable>) {}
}
