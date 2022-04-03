use std::{collections::HashMap, fs};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{
        BasicMetadataValueEnum, BasicValue, FloatValue, FunctionValue, IntValue, PointerValue,
    },
    AddressSpace,
};

use crate::{
    lexer::{self, Lexer},
    parser::{Expression, Parser, Statement, Type},
};

#[derive(Debug, Clone)]
pub enum Value<'a> {
    Float(FloatValue<'a>),
    Int(IntValue<'a>),
    Function(FunctionValue<'a>, HashMap<String, PointerValue<'a>>),
    None,
}

pub struct Variable<'a> {
    ty: Type,
    ptr: PointerValue<'a>,
}

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
    pub variables: HashMap<String, Variable<'ctx>>,
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

    pub fn compile(&mut self, ast: Vec<Statement>) {
        let mut new_ast = ast.clone();

        for statement in ast.iter() {
            if let Statement::Use(file) = statement.clone() {
                let source = fs::read_to_string(file).unwrap();
                let mut lexer = Lexer::new(&source);
                let mut parser = Parser::new(&mut lexer);

                let mut file_ast = parser.parse().unwrap();

                file_ast.append(&mut new_ast);

                new_ast = file_ast;
            }
        }

        for statement in new_ast {
            self.compile_statement(statement).unwrap();
        }

        self.module.print_to_stderr();
    }

    pub fn compile_statement(&mut self, statement: Statement) -> Result<Value<'ctx>, &'static str> {
        match statement {
            Statement::Fn {
                id,
                parameters,
                block,
                ret_type,
            } => {
                self.variables.clear();
                let mut pars = Vec::new();
                for par in parameters.iter() {
                    if let Some(t) = par.ty.to_type(self.context) {
                        pars.push(t);
                    }
                }
                let function = self.module.add_function(
                    id.as_str(),
                    ret_type.to_function_type(self.context, pars.as_slice()),
                    None,
                );

                for (index, arg) in function.get_param_iter().enumerate() {
                    arg.set_name(parameters.get(index).unwrap().id.as_str());
                }

                if let Some(Statement::Block(block)) = block.as_deref() {
                    let bl = self.context.append_basic_block(function, "entry");

                    self.builder.position_at_end(bl);

                    for (index, arg) in function.get_param_iter().enumerate() {
                        arg.set_name(parameters.get(index).unwrap().id.clone().as_str());
                        let alloca = self.builder.build_alloca(
                            arg.get_type(),
                            parameters.get(index).unwrap().id.clone().as_str(),
                        );
                        self.builder.build_store(alloca, arg);
                        self.variables.insert(
                            parameters.get(index).unwrap().id.clone(),
                            Variable {
                                ty: parameters.get(index).unwrap().ty.clone(),
                                ptr: alloca,
                            },
                        );
                    }

                    for stmt in block.iter() {
                        self.compile_statement(stmt.clone())?;
                    }
                }
            }
            Statement::Expr(expr) => match self.compile_expr(expr)? {
                _ => (),
            },
            Statement::Let { name, expr, ty } => {
                let alloca = match ty {
                    crate::parser::Type::F64 => {
                        self.builder.build_alloca(self.context.f64_type(), &name)
                    }
                    crate::parser::Type::F32 => {
                        self.builder.build_alloca(self.context.f32_type(), &name)
                    }
                    crate::parser::Type::I64 => {
                        self.builder.build_alloca(self.context.i64_type(), &name)
                    }
                    crate::parser::Type::I32 => {
                        self.builder.build_alloca(self.context.i32_type(), &name)
                    }
                    crate::parser::Type::I16 => {
                        self.builder.build_alloca(self.context.i16_type(), &name)
                    }
                    crate::parser::Type::I8 => {
                        self.builder.build_alloca(self.context.i8_type(), &name)
                    }
                    crate::parser::Type::U64 => {
                        self.builder.build_alloca(self.context.i64_type(), &name)
                    }
                    crate::parser::Type::U32 => {
                        self.builder.build_alloca(self.context.i32_type(), &name)
                    }
                    crate::parser::Type::U16 => {
                        self.builder.build_alloca(self.context.i16_type(), &name)
                    }
                    crate::parser::Type::U8 => {
                        self.builder.build_alloca(self.context.i8_type(), &name)
                    }
                    crate::parser::Type::Char => {
                        self.builder.build_alloca(self.context.i8_type(), &name)
                    }
                    crate::parser::Type::CharPtr => self.builder.build_alloca(
                        self.context.i8_type().ptr_type(AddressSpace::Generic),
                        &name,
                    ),
                    crate::parser::Type::Void => todo!(),
                };
                self.variables
                    .insert(name.clone(), Variable { ty, ptr: alloca });

                let expr = self.compile_expr(expr)?;

                match expr {
                    Value::Float(f) => {
                        self.builder
                            .build_store(self.variables.get(name.as_str()).unwrap().ptr, f);
                    }
                    Value::Int(i) => {
                        self.builder
                            .build_store(self.variables.get(name.as_str()).unwrap().ptr, i);
                    }
                    Value::Function(_, _) => todo!(),
                    Value::None => todo!(),
                }
            }
            Statement::Ret(expr) => match self.compile_expr(expr)? {
                Value::Float(val) => {
                    self.builder.build_return(Some(&val));
                }
                Value::Int(val) => {
                    self.builder.build_return(Some(&val));
                }
                Value::Function(_, _) => todo!(),
                Value::None => todo!(),
            },
            _ => (),
        }

        Ok(Value::None)
    }

    pub fn compile_expr(&mut self, expr: Expression) -> Result<Value<'ctx>, &'static str> {
        match expr {
            Expression::Number(val) => match val {
                crate::parser::Number::F64(n) => {
                    Ok(Value::Float(self.context.f64_type().const_float(n)))
                }
                crate::parser::Number::F32(n) => {
                    Ok(Value::Float(self.context.f32_type().const_float(n as f64)))
                }
                crate::parser::Number::I64(n) => Ok(Value::Int(
                    self.context.i64_type().const_int(n as u64, true),
                )),
                crate::parser::Number::I32(n) => Ok(Value::Int(
                    self.context.i32_type().const_int(n as u64, true),
                )),
                crate::parser::Number::I16(n) => Ok(Value::Int(
                    self.context.i16_type().const_int(n as u64, true),
                )),
                crate::parser::Number::I8(n) => {
                    Ok(Value::Int(self.context.i8_type().const_int(n as u64, true)))
                }
                crate::parser::Number::U64(n) => {
                    Ok(Value::Int(self.context.i64_type().const_int(n, false)))
                }
                crate::parser::Number::U32(n) => Ok(Value::Int(
                    self.context.i32_type().const_int(n as u64, false),
                )),
                crate::parser::Number::U16(n) => Ok(Value::Int(
                    self.context.i16_type().const_int(n as u64, false),
                )),
                crate::parser::Number::U8(n) => Ok(Value::Int(
                    self.context.i8_type().const_int(n as u64, false),
                )),
            },
            Expression::BinaryExpr { lhs, op, rhs } => {
                let rhs = self.compile_expr(*rhs)?;
                if let Expression::Identifier(id) = *lhs.clone() {
                    if let lexer::Token::Assign = op.token() {
                        match rhs {
                            Value::Float(f) => {
                                self.builder
                                    .build_store(self.variables.get(id.as_str()).unwrap().ptr, f);
                            }
                            Value::Int(i) => {
                                self.builder
                                    .build_store(self.variables.get(id.as_str()).unwrap().ptr, i);
                            }
                            Value::Function(_, _) => todo!(),
                            Value::None => todo!(),
                        }
                    }
                }

                let lhs = self.compile_expr(*lhs)?;

                match op.token() {
                    crate::lexer::Token::Plus => match lhs {
                        Value::Float(f) => {
                            if let Value::Float(f1) = rhs {
                                return Ok(Value::Float(
                                    self.builder.build_float_add(f, f1, "tmp"),
                                ));
                            }

                            Ok(Value::None)
                        }
                        Value::Int(i) => {
                            if let Value::Int(i1) = rhs {
                                return Ok(Value::Int(self.builder.build_int_add(i, i1, "tmp")));
                            }

                            Ok(Value::None)
                        }
                        Value::Function(_, _) => Ok(Value::None),
                        Value::None => Ok(Value::None),
                    },
                    crate::lexer::Token::Minus => Ok(Value::None),
                    crate::lexer::Token::Star => Ok(Value::None),
                    crate::lexer::Token::Slash => Ok(Value::None),
                    _ => Ok(Value::None),
                }
            }
            Expression::Grouping(expr) => self.compile_expr(*expr),
            Expression::Identifier(id) => match self.variables.get(&id).unwrap().ty {
                Type::F64 => Ok(Value::Float(
                    self.builder
                        .build_load(self.variables.get(id.as_str()).unwrap().ptr, id.as_str())
                        .into_float_value(),
                )),
                Type::F32 => Ok(Value::Float(
                    self.builder
                        .build_load(self.variables.get(id.as_str()).unwrap().ptr, id.as_str())
                        .into_float_value(),
                )),
                Type::I64 => Ok(Value::Int(
                    self.builder
                        .build_load(self.variables.get(id.as_str()).unwrap().ptr, id.as_str())
                        .into_int_value(),
                )),
                Type::I32 => Ok(Value::Int(
                    self.builder
                        .build_load(self.variables.get(id.as_str()).unwrap().ptr, id.as_str())
                        .into_int_value(),
                )),
                Type::I16 => Ok(Value::Int(
                    self.builder
                        .build_load(self.variables.get(id.as_str()).unwrap().ptr, id.as_str())
                        .into_int_value(),
                )),
                Type::I8 => Ok(Value::Int(
                    self.builder
                        .build_load(self.variables.get(id.as_str()).unwrap().ptr, id.as_str())
                        .into_int_value(),
                )),
                Type::U64 => Ok(Value::Int(
                    self.builder
                        .build_load(self.variables.get(id.as_str()).unwrap().ptr, id.as_str())
                        .into_int_value(),
                )),
                Type::U32 => Ok(Value::Int(
                    self.builder
                        .build_load(self.variables.get(id.as_str()).unwrap().ptr, id.as_str())
                        .into_int_value(),
                )),
                Type::U16 => Ok(Value::Int(
                    self.builder
                        .build_load(self.variables.get(id.as_str()).unwrap().ptr, id.as_str())
                        .into_int_value(),
                )),
                Type::U8 => Ok(Value::Int(
                    self.builder
                        .build_load(self.variables.get(id.as_str()).unwrap().ptr, id.as_str())
                        .into_int_value(),
                )),
                Type::Char => Ok(Value::Int(
                    self.builder
                        .build_load(self.variables.get(id.as_str()).unwrap().ptr, id.as_str())
                        .into_int_value(),
                )),
                Type::CharPtr => Ok(Value::Int(
                    self.builder
                        .build_load(self.variables.get(id.as_str()).unwrap().ptr, id.as_str())
                        .into_int_value(),
                )),
                Type::Void => todo!(),
            },
            Expression::Call {
                id,
                parameters,
                ret_type: _,
            } => {
                let mut compiled_args = Vec::new();

                for arg in parameters {
                    let arg = self.compile_expr(arg)?;
                    compiled_args.push(arg);
                }

                let argsv: Vec<BasicMetadataValueEnum> = compiled_args
                    .iter()
                    .by_ref()
                    .map(|val| match val {
                        Value::Float(f) => f.clone().into(),
                        Value::Int(i) => i.clone().into(),
                        Value::Function(_, _) => todo!(),
                        Value::None => todo!(),
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
            _ => Ok(Value::None),
        }
    }
}
