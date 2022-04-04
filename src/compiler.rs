use std::{collections::HashMap, fs};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::BasicMetadataTypeEnum,
    values::{
        BasicMetadataValueEnum, BasicValue, CallableValue, FloatValue, FunctionValue, IntValue,
        PointerValue,
    },
    AddressSpace, FloatPredicate, InlineAsmDialect, IntPredicate,
};

use crate::{
    lexer::{self, Lexer},
    parser::{Expression, Parser, Statement, Type},
};

#[derive(Debug, Clone)]
pub enum Value<'a> {
    Float(FloatValue<'a>),
    Int(IntValue<'a>),
    Pointer(PointerValue<'a>),
    Function(FunctionValue<'a>, HashMap<String, PointerValue<'a>>),
    Global(PointerValue<'a>),
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
    pub callables: HashMap<String, PointerValue<'ctx>>,
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
            callables: HashMap::new(),
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
                let mut pars: Vec<BasicMetadataTypeEnum> = Vec::new();
                for par in parameters.iter() {
                    if let Some(t) = par.ty.to_type(self.context) {
                        pars.push(t.into());
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

                    let mut ret = false;

                    for stmt in block.iter() {
                        if let Statement::Ret(_) = stmt {
                            ret = true;
                        }
                        self.compile_statement(stmt.clone())?;
                    }

                    if !ret {
                        self.builder.build_return(None);
                    }
                }
            }
            Statement::Asm {
                id,
                parameters,
                asm,
                ret_type,
            } => {
                self.variables.clear();
                let mut pars: Vec<BasicMetadataTypeEnum> = Vec::new();
                for par in parameters.iter() {
                    if let Some(t) = par.ty.to_type(self.context) {
                        pars.push(t.into());
                    }
                }

                let mut regs = String::from("=r");

                for par in parameters.iter() {
                    regs.push_str(&format!(",{{{}}}", par.id))
                }

                let function = self.context.create_inline_asm(
                    ret_type.to_function_type(self.context, pars.as_slice()),
                    asm,
                    regs,
                    true,
                    false,
                    Some(InlineAsmDialect::Intel),
                    false,
                );

                self.callables.insert(id, function);
            }
            Statement::Expr(expr) => match self.compile_expr(expr)? {
                _ => (),
            },
            Statement::Let { name, expr, ty } => {
                let expr = self.compile_expr(expr)?;

                let alloca = match expr {
                    Value::Float(f) => self.builder.build_alloca(f.get_type(), &name),
                    Value::Int(i) => self.builder.build_alloca(i.get_type(), &name),
                    Value::Pointer(p) => self.builder.build_alloca(p.get_type(), &name),
                    Value::Function(_, _) => todo!(),
                    Value::None => todo!(),
                    Value::Global(p) => self.builder.build_alloca(p.get_type(), &name),
                };

                self.variables
                    .insert(name.clone(), Variable { ty, ptr: alloca });

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
                    Value::Pointer(p) => {
                        self.builder
                            .build_store(self.variables.get(name.as_str()).unwrap().ptr, p);
                    }
                    Value::Global(_) => (),
                }
            }
            Statement::If { expr, block, el } => {
                let expr = self.compile_expr(expr)?;

                if let Value::Int(i) = expr {
                    let cond = self.builder.build_int_compare(
                        IntPredicate::NE,
                        i,
                        self.context.bool_type().const_int(0, true),
                        "cond",
                    );
                    let function = self
                        .builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap();

                    let then = self.context.append_basic_block(function, "then");
                    let els = self.context.append_basic_block(function, "else");
                    let merge = self.context.append_basic_block(function, "ifcont");

                    self.builder.build_conditional_branch(cond, then, els);

                    self.builder.position_at_end(then);

                    self.compile_statement(*block)?;

                    self.builder.build_unconditional_branch(merge);

                    self.builder.position_at_end(els);

                    if let Some(st) = el {
                        self.compile_statement(*st)?;
                    }

                    self.builder.build_unconditional_branch(merge);

                    self.builder.position_at_end(merge);
                }
            }
            Statement::While { expr, block } => {

                let function = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();

                let header = self.context.append_basic_block(function, "while_header");
                let then = self.context.append_basic_block(function, "then");
                let merge = self.context.append_basic_block(function, "whilecont");

                self.builder.build_unconditional_branch(header);

                self.builder.position_at_end(header);

                let expr = self.compile_expr(expr)?;

                if let Value::Int(i) = expr {
                    let cond = self.builder.build_int_compare(
                        IntPredicate::NE,
                        i,
                        self.context.bool_type().const_int(0, true),
                        "cond",
                    );

                    self.builder.build_conditional_branch(cond, then, merge);

                    self.builder.position_at_end(then);

                    self.compile_statement(*block)?;

                    self.builder.build_unconditional_branch(header);

                    self.builder.position_at_end(merge);
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
                Value::Pointer(val) => {
                    self.builder.build_return(Some(&val));
                }
                Value::Global(val) => {
                    self.builder.build_return(Some(&val));
                }
            },
            Statement::Block(bl) => {
                for st in bl.iter() {
                    self.compile_statement(st.clone())?;
                }
            }
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
            Expression::String(s) => {
                let glob = self.module.add_global(
                    self.context.i8_type().array_type((s.len() + 1) as u32),
                    Some(AddressSpace::Const),
                    "str",
                );
                glob.set_initializer(&self.context.const_string(s.as_bytes(), true));
                Ok(Value::Pointer(glob.as_pointer_value()))
            }
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
                            Value::Pointer(p) => {
                                let alloca = self.builder.build_alloca(p.get_type(), id.as_str());
                                self.builder.build_store(alloca, p);
                                self.variables.get_mut(&id).unwrap().ptr = alloca;
                            }
                            Value::Global(p) => {
                                self.builder
                                    .build_store(self.variables.get(id.as_str()).unwrap().ptr, p);
                            }
                        }
                    }
                }

                let lhs = self.compile_expr(*lhs)?;

                match op.token() {
                    crate::lexer::Token::Greater => match lhs {
                        Value::Float(f) => {
                            if let Value::Float(f1) = rhs {
                                return Ok(Value::Int(
                                    self.builder
                                        .build_float_compare(FloatPredicate::OGT, f, f1, "tmpcmp")
                                        .as_basic_value_enum()
                                        .into_int_value(),
                                ));
                            } else {
                                return Ok(Value::None);
                            }
                        }
                        Value::Int(i) => {
                            if let Value::Int(i1) = rhs {
                                return Ok(Value::Int(
                                    self.builder
                                        .build_int_compare(IntPredicate::SGT, i, i1, "tmpcmp")
                                        .as_basic_value_enum()
                                        .into_int_value(),
                                ));
                            } else {
                                return Ok(Value::None);
                            }
                        }
                        _ => return Ok(Value::None),
                    },
                    crate::lexer::Token::Less => match lhs {
                        Value::Float(f) => {
                            if let Value::Float(f1) = rhs {
                                return Ok(Value::Int(
                                    self.builder
                                        .build_float_compare(FloatPredicate::OLT, f, f1, "tmpcmp")
                                        .as_basic_value_enum()
                                        .into_int_value(),
                                ));
                            } else {
                                return Ok(Value::None);
                            }
                        }
                        Value::Int(i) => {
                            if let Value::Int(i1) = rhs {
                                return Ok(Value::Int(
                                    self.builder
                                        .build_int_compare(IntPredicate::SLT, i, i1, "tmpcmp")
                                        .as_basic_value_enum()
                                        .into_int_value(),
                                ));
                            } else {
                                return Ok(Value::None);
                            }
                        }
                        _ => return Ok(Value::None),
                    },
                    crate::lexer::Token::Equal => match lhs {
                        Value::Float(f) => {
                            if let Value::Float(f1) = rhs {
                                return Ok(Value::Int(
                                    self.builder
                                        .build_float_compare(FloatPredicate::OEQ, f, f1, "tmpcmp")
                                        .as_basic_value_enum()
                                        .into_int_value(),
                                ));
                            } else {
                                return Ok(Value::None);
                            }
                        }
                        Value::Int(i) => {
                            if let Value::Int(i1) = rhs {
                                return Ok(Value::Int(
                                    self.builder
                                        .build_int_compare(IntPredicate::EQ, i, i1, "tmpcmp")
                                        .as_basic_value_enum()
                                        .into_int_value(),
                                ));
                            } else {
                                return Ok(Value::None);
                            }
                        }
                        _ => return Ok(Value::None),
                    },
                    crate::lexer::Token::NotEqual => match lhs {
                        Value::Float(f) => {
                            if let Value::Float(f1) = rhs {
                                return Ok(Value::Int(
                                    self.builder
                                        .build_float_compare(FloatPredicate::ONE, f, f1, "tmpcmp")
                                        .as_basic_value_enum()
                                        .into_int_value(),
                                ));
                            } else {
                                return Ok(Value::None);
                            }
                        }
                        Value::Int(i) => {
                            if let Value::Int(i1) = rhs {
                                return Ok(Value::Int(
                                    self.builder
                                        .build_int_compare(IntPredicate::NE, i, i1, "tmpcmp")
                                        .as_basic_value_enum()
                                        .into_int_value(),
                                ));
                            } else {
                                return Ok(Value::None);
                            }
                        }
                        _ => return Ok(Value::None),
                    },
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
                        Value::Pointer(p) => {
                            if let Value::Int(i) = rhs {
                                let p1 = self.builder.build_ptr_to_int(
                                    p,
                                    self.context.i64_type(),
                                    "tmp",
                                );
                                let p1 = self.builder.build_int_add(p1, i, "tmpadd");
                                let sum = self.builder.build_int_to_ptr(p1, p.get_type(), "tmpptr");
                                return Ok(Value::Pointer(sum));
                            }

                            Ok(Value::None)
                        }
                        Value::Global(p) => {
                            if let Value::Int(i) = rhs {
                                let p1 = self.builder.build_ptr_to_int(p, i.get_type(), "tmp");
                                let sum = self.builder.build_int_to_ptr(p1, p.get_type(), "tmpptr");
                                return Ok(Value::Pointer(sum));
                            }

                            Ok(Value::None)
                        }
                    },
                    crate::lexer::Token::Minus => Ok(Value::None),
                    crate::lexer::Token::Star => Ok(Value::None),
                    crate::lexer::Token::Slash => Ok(Value::None),
                    _ => Ok(Value::None),
                }
            }
            Expression::Char(c) => Ok(Value::Int(self.context.i8_type().const_int(c as u64, true))),
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
                Type::CharPtr => Ok(Value::Pointer(
                    self.builder.build_int_to_ptr(
                        self.builder.build_ptr_to_int(
                            self.builder
                                .build_load(self.variables.get(id.as_str()).unwrap().ptr, "tmpload")
                                .into_pointer_value(),
                            self.context.i64_type(),
                            "tmpcast",
                        ),
                        self.context.i8_type().ptr_type(AddressSpace::Generic),
                        "tmpcast",
                    ),
                )),

                Type::Void => todo!(),
                Type::Bool => Ok(Value::Int(
                    self.builder
                        .build_load(self.variables.get(id.as_str()).unwrap().ptr, id.as_str())
                        .into_int_value(),
                )),
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
                        Value::Pointer(p) => p.clone().into(),
                        Value::Global(p) => p.clone().into(),
                    })
                    .collect();

                if let Some(c) = self.callables.get(&id) {
                    self.builder.build_call(
                        CallableValue::try_from(c.clone()).unwrap(),
                        argsv.as_slice(),
                        "tmp",
                    );
                    return Ok(Value::None);
                }

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
                        inkwell::values::BasicValueEnum::IntValue(i) => return Ok(Value::Int(i)),
                        inkwell::values::BasicValueEnum::FloatValue(f) => {
                            return Ok(Value::Float(f))
                        }
                        inkwell::values::BasicValueEnum::PointerValue(_) => todo!(),
                        inkwell::values::BasicValueEnum::StructValue(_) => todo!(),
                        inkwell::values::BasicValueEnum::VectorValue(_) => todo!(),
                    },
                    None => return Ok(Value::None),
                }
            }
            Expression::UnaryExpr { op, rhs } => match op.token() {
                lexer::Token::And => {
                    if let Expression::Identifier(id) = *rhs {
                        if let Some(var) = self.variables.get(&id) {
                            return Ok(Value::Pointer(var.ptr));
                        }
                    }

                    Ok(Value::None)
                }
                _ => Ok(Value::None),
            },
            Expression::True => Ok(Value::Int(self.context.bool_type().const_int(1, false))),
            Expression::False => Ok(Value::Int(self.context.bool_type().const_int(0, false))),
        }
    }
}
