use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{FloatValue, FunctionValue, IntValue},
};

use crate::ast::AstNode;

#[derive(Debug, Clone)]
pub enum Value<'a> {
    Float(FloatValue<'a>),
    Int(IntValue<'a>),
    Function(FunctionValue<'a>),
    None,
}

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn new(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        module: &'a Module<'ctx>,
    ) -> Self {
        Self {
            builder: builder,
            context,
            module,
        }
    }

    pub fn compile(&mut self, ast: Vec<AstNode>) {
        let entry =
            self.module
                .add_function("start", self.context.void_type().fn_type(&[], false), None);
        let block = self.context.append_basic_block(entry, "entry");
        self.builder.position_at_end(block);

        for statement in ast {
            self.compile_statement(statement).unwrap();
        }

        self.module.print_to_stderr();
    }

    pub fn compile_statement(&mut self, statement: AstNode) -> Result<Value<'ctx>, &'static str> {
        match statement {
            AstNode::Call { id, args } => todo!(),
            AstNode::Definition {
                id,
                parameters,
                block,
            } => {
                if let AstNode::Identifier(id) = *id {
                    if let AstNode::Parameters(params, ret_type) = *parameters {
                        if let AstNode::Block(block) = *block {
                            let mut pars = Vec::new();
                            for par in params.iter() {
                                if let AstNode::Parameter(_id, ty) = par {
                                    if let Some(t) = ty.to_type(self.context) {
                                        pars.push(t);
                                    }
                                }
                            }
                            let function = self.module.add_function(
                                id.as_str(),
                                ret_type.to_function_type(self.context, pars.as_slice()),
                                None,
                            );
                            let bl = self.context.append_basic_block(function, "entry");
                            self.builder.position_at_end(bl);
                            for stmt in block.iter() {
                                self.compile_statement(stmt.clone())?;
                            }
                        }
                    }
                }
            }
            AstNode::ExprStatement(expr) => match self.compile_expr(*expr).unwrap() {
                Value::Float(f) => {
                    self.builder.build_return(Some(&f));
                }
                Value::Int(i) => todo!(),
                Value::Function(f) => todo!(),
                Value::None => todo!(),
            },
            AstNode::LetStatement(_) => todo!(),
            AstNode::RetStatement(expr) => match self.compile_expr(*expr)? {
                Value::Float(val) => {
                    self.builder.build_return(Some(&val));
                }
                Value::Int(_) => todo!(),
                Value::Function(_) => todo!(),
                Value::None => todo!(),
            },
            _ => (),
        }

        Ok(Value::None)
    }

    pub fn compile_expr(&mut self, expr: AstNode) -> Result<Value<'ctx>, &'static str> {
        match expr {
            AstNode::Number(n) => Ok(Value::Float(self.context.f64_type().const_float(n))),
            AstNode::BinaryExpr { lhs, op, rhs } => {
                let lhs = self.compile_expr(*lhs)?;
                let rhs = self.compile_expr(*rhs)?;

                match op {
                    crate::lexer::Token::Plus => {
                        if let Value::Float(lhs) = lhs {
                            if let Value::Float(rhs) = rhs {
                                Ok(Value::Float(
                                    self.builder.build_float_add(lhs, rhs, "tmpadd"),
                                ))
                            } else {
                                Ok(Value::None)
                            }
                        } else {
                            Ok(Value::None)
                        }
                    }
                    crate::lexer::Token::Minus => Ok(Value::None),
                    crate::lexer::Token::Star => Ok(Value::None),
                    crate::lexer::Token::Slash => Ok(Value::None),
                    _ => Ok(Value::None),
                }
            }
            AstNode::None => Ok(Value::None),
            _ => Ok(Value::None),
        }
    }
}
