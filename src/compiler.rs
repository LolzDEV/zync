use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{
        BasicMetadataValueEnum, BasicValue, FloatValue, FunctionValue, IntValue, PointerValue,
    },
};

use crate::{ast::AstNode, lexer::Token};

#[derive(Debug, Clone)]
pub enum Value<'a> {
    Float(FloatValue<'a>),
    Int(IntValue<'a>),
    Function(FunctionValue<'a>, HashMap<String, PointerValue<'a>>),
    None,
}

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
    pub variables: HashMap<String, PointerValue<'ctx>>,
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
            variables: HashMap::new(),
        }
    }

    pub fn compile(&mut self, ast: Vec<AstNode>) {
        for statement in ast {
            self.compile_statement(statement).unwrap();
        }

        self.module.print_to_stderr();
    }

    pub fn compile_statement(&mut self, statement: AstNode) -> Result<Value<'ctx>, &'static str> {
        match statement {
            AstNode::Definition {
                id,
                parameters,
                block,
            } => {
                if let AstNode::Identifier(id) = *id {
                    if let AstNode::Parameters(params, ret_type) = *parameters {
                        self.variables.clear();
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

                        for (index, arg) in function.get_param_iter().enumerate() {
                            if let Some(AstNode::Parameter(id, _ty)) = params.iter().nth(index) {
                                if let AstNode::Identifier(id) = *id.clone() {
                                    arg.into_float_value().set_name(id.clone().as_str());
                                }
                            }
                        }

                        if let AstNode::Block(block) = *block {
                            let bl = self.context.append_basic_block(function, "entry");

                            self.builder.position_at_end(bl);

                            for (index, arg) in function.get_param_iter().enumerate() {
                                if let Some(AstNode::Parameter(id, _ty)) = params.iter().nth(index)
                                {
                                    if let AstNode::Identifier(id) = *id.clone() {
                                        arg.into_float_value().set_name(id.clone().as_str());
                                        let alloca =
                                            self.builder.build_alloca(arg.get_type(), id.as_str());
                                        self.builder.build_store(alloca, arg);
                                        self.variables.insert(id, alloca);
                                    }
                                }
                            }
                            for stmt in block.iter() {
                                self.compile_statement(stmt.clone())?;
                            }
                        }
                    }
                }
            }
            AstNode::ExprStatement(expr) => match self.compile_expr(*expr)? {
                _ => (),
            },
            AstNode::LetStatement(expr) => {
                if let AstNode::BinaryExpr {
                    lhs,
                    op: Token::Assign,
                    rhs: _,
                } = *expr.clone()
                {
                    if let AstNode::Identifier(id) = *lhs {
                        self.variables.insert(
                            id.clone(),
                            self.builder.build_alloca(self.context.f64_type(), &id),
                        );
                        self.compile_expr(*expr)?;
                    }
                }
            }
            AstNode::RetStatement(expr) => match self.compile_expr(*expr)? {
                Value::Float(val) => {
                    self.builder.build_return(Some(&val));
                }
                Value::Int(_) => todo!(),
                Value::Function(_, _) => todo!(),
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
                let rhs = self.compile_expr(*rhs)?;
                if let AstNode::Identifier(id) = *lhs.clone() {
                    if let Token::Assign = op {
                        if let Value::Float(val) = rhs {
                            self.builder
                                .build_store(*self.variables.get(id.as_str()).unwrap(), val);
                        }
                    }
                }

                let lhs = self.compile_expr(*lhs)?;

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
            AstNode::Grouping(expr) => self.compile_expr(*expr),
            AstNode::Identifier(id) => Ok(Value::Float(
                self.builder
                    .build_load(*self.variables.get(id.as_str()).unwrap(), id.as_str())
                    .into_float_value(),
            )),
            AstNode::Call { id, args } => {
                if let AstNode::Identifier(id) = *id {
                    if let AstNode::Arguments(args) = *args {
                        let mut compiled_args = Vec::new();

                        for arg in args {
                            let arg = self.compile_expr(arg)?;
                            compiled_args.push(arg);
                        }

                        let argsv: Vec<BasicMetadataValueEnum> = compiled_args
                            .iter()
                            .by_ref()
                            .map(|val| {
                                if let Value::Float(val) = *val {
                                    val.into()
                                } else {
                                    self.context.f64_type().const_float(0.).into()
                                }
                            })
                            .collect();

                        match self
                            .builder
                            .build_call(
                                self.module
                                    .get_function(&id)
                                    .expect("Called non-extistent function."),
                                argsv.as_slice(),
                                "tmp",
                            )
                            .try_as_basic_value()
                            .left()
                        {
                            Some(value) => match value {
                                inkwell::values::BasicValueEnum::ArrayValue(_) => todo!(),
                                inkwell::values::BasicValueEnum::IntValue(_) => todo!(),
                                inkwell::values::BasicValueEnum::FloatValue(f) => {
                                    return Ok(Value::Float(f))
                                }
                                inkwell::values::BasicValueEnum::PointerValue(_) => todo!(),
                                inkwell::values::BasicValueEnum::StructValue(_) => todo!(),
                                inkwell::values::BasicValueEnum::VectorValue(_) => todo!(),
                            },
                            None => todo!(),
                        }
                    }
                }

                todo!()
            }
            AstNode::None => Ok(Value::None),
            _ => Ok(Value::None),
        }
    }
}
