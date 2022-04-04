use std::{fmt::Display, fs};

use colored::Colorize;
use inkwell::{
    context::Context,
    types::{BasicMetadataTypeEnum, BasicTypeEnum, FunctionType},
    AddressSpace,
};

use crate::lexer::{self, FilePosition, Lexer};

#[derive(Debug, Clone)]
pub enum Error {
    ParserError {
        at: FilePosition,
        message: String,
    },
    MismatchedTypes {
        expected: Type,
        actual: Type,
        at: FilePosition,
    },
    NoFn(String, FilePosition),
    EndOfFile,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::ParserError { at, message } => write!(
                f,
                "{} {}:{} -> {}",
                "error:".red(),
                at.line,
                at.column,
                message
            ),
            Error::EndOfFile => write!(f, "EOF"),
            Error::MismatchedTypes {
                expected,
                actual,
                at,
            } => write!(
                f,
                "{} {}:{} -> Mismatched types, expected: {:?} but found {:?}",
                "error:".red(),
                at.line,
                at.column,
                expected,
                actual,
            ),
            Error::NoFn(name, pos) => write!(
                f,
                "{} {}:{} -> No function `{}` found",
                "error:".red(),
                pos.line,
                pos.column,
                name
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    F64,
    F32,
    I64,
    I32,
    I16,
    I8,
    U64,
    U32,
    U16,
    U8,
    Char,
    CharPtr,
    Bool,
    Void,
}

impl Type {
    pub fn to_type<'ctx>(&self, context: &'ctx Context) -> Option<BasicTypeEnum<'ctx>> {
        match self {
            Type::F64 => Some(context.f64_type().into()),
            Type::F32 => Some(context.f32_type().into()),
            Type::I64 => Some(context.i64_type().into()),
            Type::I32 => Some(context.i32_type().into()),
            Type::I16 => Some(context.i16_type().into()),
            Type::I8 => Some(context.i8_type().into()),
            Type::U64 => Some(context.i64_type().into()),
            Type::U32 => Some(context.i32_type().into()),
            Type::U16 => Some(context.i16_type().into()),
            Type::U8 => Some(context.i8_type().into()),
            Type::Char => Some(context.i8_type().into()),
            Type::CharPtr => Some(context.i8_type().ptr_type(AddressSpace::Generic).into()),
            Type::Void => None,
            Type::Bool => Some(context.i8_type().into()),
        }
    }

    pub fn to_function_type<'ctx>(
        &self,
        context: &'ctx Context,
        params: &[BasicMetadataTypeEnum<'ctx>],
    ) -> FunctionType<'ctx> {
        match self {
            Type::F64 => context.f64_type().fn_type(params, false),
            Type::F32 => context.f32_type().fn_type(params, false),
            Type::I64 => context.i64_type().fn_type(params, false),
            Type::I32 => context.i32_type().fn_type(params, false),
            Type::I16 => context.i16_type().fn_type(params, false),
            Type::I8 => context.i8_type().fn_type(params, false),
            Type::U64 => context.i64_type().fn_type(params, false),
            Type::U32 => context.i32_type().fn_type(params, false),
            Type::U16 => context.i16_type().fn_type(params, false),
            Type::U8 => context.i8_type().fn_type(params, false),
            Type::Void => context.void_type().fn_type(params, false),
            Type::Bool => context.i8_type().fn_type(params, false),
            Type::Char => context.i8_type().fn_type(params, false),
            Type::CharPtr => context
                .i8_type()
                .ptr_type(AddressSpace::Generic)
                .fn_type(params, false),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Number {
    F64(f64),
    F32(f32),
    I64(i64),
    I32(i32),
    I16(i16),
    I8(i8),
    U64(u64),
    U32(u32),
    U16(u16),
    U8(u8),
}

impl Number {
    pub fn get_type(&self) -> Type {
        match self {
            Number::F64(_) => Type::F64,
            Number::F32(_) => Type::F32,
            Number::I64(_) => Type::I64,
            Number::I32(_) => Type::I32,
            Number::I16(_) => Type::I16,
            Number::I8(_) => Type::I8,
            Number::U64(_) => Type::U64,
            Number::U32(_) => Type::U32,
            Number::U16(_) => Type::U16,
            Number::U8(_) => Type::U8,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Expression),
    Let {
        name: String,
        expr: Expression,
        ty: Type,
    },
    Fn {
        id: String,
        parameters: Vec<Parameter>,
        block: Option<Box<Statement>>,
        ret_type: Type,
    },
    Asm {
        id: String,
        parameters: Vec<Parameter>,
        asm: String,
        ret_type: Type,
    },
    Block(Vec<Statement>),
    Ret(Expression),
    Use(String),
    If {
        expr: Expression,
        block: Box<Statement>,
        el: Option<Box<Statement>>,
    },
    While {
        expr: Expression,
        block: Box<Statement>,
    },
}

#[derive(Debug, Clone)]
pub enum Expression {
    BinaryExpr {
        lhs: Box<Expression>,
        op: Token,
        rhs: Box<Expression>,
    },
    UnaryExpr {
        op: Token,
        rhs: Box<Expression>,
    },
    Number(Number),
    Identifier(String),
    Grouping(Box<Expression>),
    Call {
        id: String,
        parameters: Vec<Expression>,
        ret_type: Type,
    },
    True,
    False,
    Char(char),
    String(String),
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub id: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub tok: lexer::Token,
    pub position: FilePosition,
}

impl Token {
    pub fn token(&self) -> lexer::Token {
        self.tok.clone()
    }

    pub fn position(&self) -> FilePosition {
        self.position.clone()
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub parameters: Vec<Parameter>,
    pub id: String,
    pub ret_type: Type,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub ty: Type,
}

pub struct Parser {
    pub tokens: Vec<Token>,
    position: usize,
    functions: Vec<Function>,
    current_scope: Vec<Variable>,
}

impl Parser {
    pub fn new(lexer: &mut Lexer) -> Self {
        let mut tokens = Vec::new();
        loop {
            let (tok, pos) = lexer.next();
            println!("Pushed {:?}", tok);
            if let lexer::Token::Eof = tok {
                break;
            }

            tokens.push(Token { tok, position: pos });
        }

        Self {
            tokens,
            position: 0,
            functions: Vec::new(),
            current_scope: Vec::new(),
        }
    }

    pub fn next_token(&mut self) -> Result<Token, Error> {
        let tok = self.tokens.get(self.position).ok_or(Error::EndOfFile)?;
        self.position += 1;
        Ok(tok.clone())
    }

    pub fn check(&mut self, token: lexer::Token) -> Result<(), Error> {
        let tok = self.tokens.get(self.position).ok_or(Error::EndOfFile)?;
        if tok.token() == token {
            Ok(())
        } else {
            Err(Error::ParserError {
                at: tok.position(),
                message: format!("Expected {:?} but {:?} found", token, tok.token()),
            })
        }
    }

    pub fn expect(&mut self, tokens: Vec<lexer::Token>) -> Result<(), Error> {
        let pos = self
            .next_token()
            .unwrap_or(Token {
                tok: lexer::Token::Eof,
                position: FilePosition { line: 0, column: 0 },
            })
            .position();

        self.position -= 1;

        for tok in tokens.iter() {
            if let Ok(_) = self.check(tok.clone()) {
                self.position += 1;
                return Ok(());
            }
        }

        let mut message = String::from("Expected ");

        for tok in tokens.iter() {
            message.push_str(&format!("{:?}, ", tok))
        }

        Err(Error::ParserError { at: pos, message })
    }

    pub fn previous(&mut self) -> Result<Token, Error> {
        let tok = self
            .tokens
            .get(self.position - 1)
            .ok_or(Error::EndOfFile)?
            .clone();
        Ok(tok)
    }

    pub fn consume(&mut self, token: lexer::Token) -> Result<(), Error> {
        let tok = self.next_token()?;
        if tok.token() == token {
            return Ok(());
        } else {
            self.position -= 1;
        }

        Err(Error::ParserError {
            at: tok.position(),
            message: format!("Expected {:?} but {:?} found", token, tok.token()),
        })
    }

    pub fn evaluate_expr_type(&mut self, expr: Expression) -> Result<Type, Error> {
        match expr {
            Expression::BinaryExpr { lhs, op, rhs } => {
                match op.token() {
                    lexer::Token::Greater => return Ok(Type::Bool),
                    lexer::Token::Less => return Ok(Type::Bool),
                    lexer::Token::Equal => return Ok(Type::Bool),
                    lexer::Token::NotEqual => return Ok(Type::Bool),
                    _ => (),
                }

                let lhs = self.evaluate_expr_type(*lhs)?;
                let rhs = self.evaluate_expr_type(*rhs)?;

                if let Type::CharPtr = rhs {
                    return Ok(rhs);
                }

                if let Type::CharPtr = lhs {
                    return Ok(lhs);
                }

                if lhs == rhs {
                    return Ok(lhs);
                }

                Err(Error::MismatchedTypes {
                    expected: lhs,
                    actual: rhs,
                    at: op.position(),
                })
            }
            Expression::UnaryExpr { op, rhs } => {
                if let lexer::Token::And = op.token() {
                    if let Ok(Type::Char) = self.evaluate_expr_type(*rhs.clone()) {
                        return Ok(Type::CharPtr);
                    }
                }

                self.evaluate_expr_type(*rhs)
            }
            Expression::Number(n) => Ok(n.get_type()),
            Expression::Identifier(name) => {
                for var in self.current_scope.iter() {
                    if var.name == name {
                        return Ok(var.ty.clone());
                    }
                }
                Err(Error::ParserError {
                    at: FilePosition { line: 0, column: 0 },
                    message: format!("Variable `{}` does not exist in this scope", name),
                })
            }
            Expression::Grouping(expr) => self.evaluate_expr_type(*expr),
            Expression::Call {
                id: _,
                parameters: _,
                ret_type,
            } => Ok(ret_type),
            Expression::String(_) => Ok(Type::CharPtr),
            Expression::Char(_) => Ok(Type::Char),
            Expression::True => Ok(Type::Bool),
            Expression::False => Ok(Type::Bool),
        }
    }

    pub fn statement(&mut self) -> Result<Statement, Error> {
        if let Ok(_) = self.expect(vec![lexer::Token::Fn]) {
            return self.fn_statement();
        }

        if let Ok(_) = self.expect(vec![lexer::Token::Let]) {
            return self.let_statement();
        }

        if let Ok(_) = self.expect(vec![lexer::Token::Ret]) {
            return self.ret_statement();
        }

        if let Ok(_) = self.expect(vec![lexer::Token::Use]) {
            return self.use_statement();
        }

        if let Ok(_) = self.expect(vec![lexer::Token::Asm]) {
            return self.asm_statement();
        }

        if let Ok(_) = self.expect(vec![lexer::Token::If]) {
            return self.if_statement();
        }

        if let Ok(_) = self.expect(vec![lexer::Token::While]) {
            return self.while_statement();
        }

        self.expr_statement()
    }

    pub fn if_statement(&mut self) -> Result<Statement, Error> {
        let tok = self.next_token()?;
        self.position -= 1;
        let expr = self.expr()?;

        if let Type::Bool = self.evaluate_expr_type(expr.clone())? {
            let block = self.block()?;

            let mut el = None;

            if let Ok(_) = self.expect(vec![lexer::Token::Else]) {
                el = Some(Box::new(self.block()?));
            }

            return Ok(Statement::If {
                expr,
                block: Box::new(block),
                el,
            });
        } else {
            return Err(Error::ParserError {
                at: tok.position(),
                message: "Expected Bool".to_string(),
            });
        }
    }

    pub fn while_statement(&mut self) -> Result<Statement, Error> {
        let tok = self.next_token()?;
        self.position -= 1;
        let expr = self.expr()?;

        if let Type::Bool = self.evaluate_expr_type(expr.clone())? {
            let block = self.block()?;

            return Ok(Statement::While {
                expr,
                block: Box::new(block),
            });
        } else {
            return Err(Error::ParserError {
                at: tok.position(),
                message: "Expected Bool".to_string(),
            });
        }
    }

    pub fn use_statement(&mut self) -> Result<Statement, Error> {
        let tok = self.next_token()?;
        self.position -= 1;

        if let Expression::String(s) = self.primary()? {
            self.consume(lexer::Token::SemiColon)?;
            return Ok(Statement::Use(s));
        }

        Err(Error::ParserError {
            at: tok.position(),
            message: format!("Expected file but {:?} found", tok.token()),
        })
    }

    pub fn ret_statement(&mut self) -> Result<Statement, Error> {
        let stmt = Ok(Statement::Ret(self.expr()?));
        self.consume(lexer::Token::SemiColon)?;
        stmt
    }

    pub fn let_statement(&mut self) -> Result<Statement, Error> {
        let tok = self.next_token()?;

        if let lexer::Token::Identifier(name) = tok.token() {
            self.consume(lexer::Token::Assign)?;
            let expr = self.expr()?;
            self.consume(lexer::Token::SemiColon)?;
            let ty = self.evaluate_expr_type(expr.clone())?;

            self.current_scope.push(Variable {
                name: name.clone(),
                ty: ty.clone(),
            });

            return Ok(Statement::Let { name, expr, ty });
        }

        self.position -= 1;

        Err(Error::ParserError {
            at: tok.position(),
            message: "Expected Identifier".to_string(),
        })
    }

    pub fn fn_statement(&mut self) -> Result<Statement, Error> {
        self.current_scope.clear();

        let id = self.identifier()?;
        self.consume(lexer::Token::LeftParen)?;

        let param = self.param();

        let params = if let Ok(param) = param {
            let mut params = vec![param];

            while let Ok(_) = self.expect(vec![lexer::Token::Comma]) {
                params.push(self.param()?);
            }

            self.consume(lexer::Token::RightParen)?;

            params
        } else {
            self.consume(lexer::Token::RightParen)?;
            vec![]
        };

        let mut ret_type = Type::Void;

        if let Ok(_) = self.expect(vec![lexer::Token::ReturnArrow]) {
            ret_type = self.ty()?;
        }

        for param in params.iter() {
            self.current_scope.push(Variable {
                name: param.id.clone(),
                ty: param.ty.clone(),
            });
        }

        if let Ok(_) = self.expect(vec![lexer::Token::SemiColon]) {
            return Ok(Statement::Fn {
                id,
                parameters: params,
                block: None,
                ret_type,
            });
        }

        let block = if let Ok(block) = self.block() {
            Some(Box::new(block))
        } else {
            None
        };

        Ok(Statement::Fn {
            id,
            parameters: params,
            block: block,
            ret_type,
        })
    }

    pub fn asm_statement(&mut self) -> Result<Statement, Error> {
        self.current_scope.clear();

        let id = self.identifier()?;
        self.consume(lexer::Token::LeftParen)?;

        let param = self.param();

        let params = if let Ok(param) = param {
            let mut params = vec![param];

            while let Ok(_) = self.expect(vec![lexer::Token::Comma]) {
                params.push(self.param()?);
            }

            self.consume(lexer::Token::RightParen)?;

            params
        } else {
            self.consume(lexer::Token::RightParen)?;
            vec![]
        };

        let mut ret_type = Type::Void;

        if let Ok(_) = self.expect(vec![lexer::Token::ReturnArrow]) {
            ret_type = self.ty()?;
        }

        for param in params.iter() {
            self.current_scope.push(Variable {
                name: param.id.clone(),
                ty: param.ty.clone(),
            });
        }

        Ok(Statement::Asm {
            id,
            parameters: params,
            asm: self.asm()?,
            ret_type,
        })
    }

    pub fn asm(&mut self) -> Result<String, Error> {
        self.consume(lexer::Token::LeftBracket)?;

        let mut asm = String::new();

        while let Err(_) = self.expect(vec![lexer::Token::RightBracket]) {
            let tok = self.next_token()?;

            if let lexer::Token::String(s) = tok.token() {
                asm.push_str(&s);
            } else {
                self.position -= 1;
            }
        }

        Ok(asm)
    }

    pub fn block(&mut self) -> Result<Statement, Error> {
        self.consume(lexer::Token::LeftBracket)?;

        let mut statements = Vec::new();

        while let Err(_) = self.expect(vec![lexer::Token::RightBracket]) {
            statements.push(self.statement()?);
        }

        Ok(Statement::Block(statements))
    }

    pub fn param(&mut self) -> Result<Parameter, Error> {
        let id = self.identifier()?;
        self.consume(lexer::Token::Colon)?;
        let ty = self.ty()?;
        Ok(Parameter { id, ty })
    }

    pub fn ty(&mut self) -> Result<Type, Error> {
        let tok = self.next_token()?;

        let is_ptr = if let Ok(_) = self.expect(vec![lexer::Token::Star]) {
            true
        } else {
            false
        };

        match tok.token() {
            lexer::Token::F64Type => Ok(Type::F64),
            lexer::Token::F32Type => Ok(Type::F32),
            lexer::Token::I64Type => Ok(Type::I64),
            lexer::Token::I32Type => Ok(Type::I32),
            lexer::Token::I16Type => Ok(Type::I16),
            lexer::Token::I8Type => Ok(Type::I8),
            lexer::Token::CharType => {
                if is_ptr {
                    Ok(Type::CharPtr)
                } else {
                    Ok(Type::Char)
                }
            }
            lexer::Token::VoidType => Ok(Type::Void),
            _ => Err(Error::ParserError {
                at: tok.position(),
                message: format!("Expected Type but {:?} found", tok.token()),
            }),
        }
    }

    pub fn expr_statement(&mut self) -> Result<Statement, Error> {
        let expr = Statement::Expr(self.expr()?);
        self.consume(lexer::Token::SemiColon)?;
        Ok(expr)
    }

    pub fn expr(&mut self) -> Result<Expression, Error> {
        let ex = self.assignment()?;
        self.evaluate_expr_type(ex.clone())?;
        Ok(ex)
    }

    pub fn assignment(&mut self) -> Result<Expression, Error> {
        let mut lhs = self.comparison()?;

        while let Ok(_) = self.expect(vec![lexer::Token::Assign]) {
            let operator = self.previous()?;

            let rhs = self.term()?;
            lhs = Expression::BinaryExpr {
                lhs: Box::new(lhs),
                op: operator,
                rhs: Box::new(rhs),
            }
        }

        Ok(lhs)
    }

    pub fn comparison(&mut self) -> Result<Expression, Error> {
        let mut lhs = self.term()?;

        if let Ok(_) = self.expect(vec![
            lexer::Token::Greater,
            lexer::Token::Less,
            lexer::Token::NotEqual,
            lexer::Token::Equal,
        ]) {
            let operator = self.previous()?;

            let rhs = self.factor()?;
            lhs = Expression::BinaryExpr {
                lhs: Box::new(lhs),
                op: operator,
                rhs: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    pub fn term(&mut self) -> Result<Expression, Error> {
        let mut lhs = self.factor()?;

        while let Ok(_) = self.expect(vec![lexer::Token::Minus, lexer::Token::Plus]) {
            let operator = self.previous()?;

            let rhs = self.factor()?;
            lhs = Expression::BinaryExpr {
                lhs: Box::new(lhs),
                op: operator,
                rhs: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    pub fn factor(&mut self) -> Result<Expression, Error> {
        let mut expr = self.unary()?;

        while let Ok(_) = self.expect(vec![lexer::Token::Star, lexer::Token::Slash]) {
            let operator = self.previous()?;
            let rhs = self.unary()?;
            expr = Expression::BinaryExpr {
                lhs: Box::new(expr),
                op: operator,
                rhs: Box::new(rhs),
            }
        }

        Ok(expr)
    }

    pub fn unary(&mut self) -> Result<Expression, Error> {
        if let Ok(_) = self.expect(vec![lexer::Token::Minus, lexer::Token::And]) {
            let op = self.previous()?;
            let rhs = self.unary()?;

            return Ok(Expression::UnaryExpr {
                op: op,
                rhs: Box::new(rhs),
            });
        }

        self.primary()
    }

    pub fn identifier(&mut self) -> Result<String, Error> {
        let next = self.next_token()?;
        self.position -= 1;
        if let lexer::Token::Identifier(id) = next.token() {
            self.position += 1;
            return Ok(id);
        }

        Err(Error::ParserError {
            at: next.position(),
            message: format!("Expected Identifier but {:?} found", next.token()),
        })
    }

    pub fn arguments(&mut self) -> Result<Vec<Expression>, Error> {
        self.consume(lexer::Token::LeftParen)?;
        if let Ok(_) = self.consume(lexer::Token::RightParen) {
            return Ok(vec![]);
        }

        let arg = self.expr();

        if let Ok(arg) = arg {
            let mut args = vec![arg];

            while let Ok(_) = self.expect(vec![lexer::Token::Comma]) {
                args.push(self.expr()?);
            }

            self.consume(lexer::Token::RightParen)?;

            println!("{:?}", self.current_scope);
            println!(
                "{:?}",
                self.evaluate_expr_type(args.get(0).unwrap().clone())
            );

            Ok(args)
        } else {
            self.consume(lexer::Token::RightParen)?;

            Ok(vec![])
        }
    }

    pub fn primary(&mut self) -> Result<Expression, Error> {
        if let lexer::Token::Float(n) = self.next_token()?.token() {
            if let Ok(_) = self.expect(vec![lexer::Token::F32Type]) {
                return Ok(Expression::Number(Number::F32(n as f32)));
            }
            if let Ok(_) = self.expect(vec![lexer::Token::F64Type]) {
                return Ok(Expression::Number(Number::F64(n)));
            }

            return Ok(Expression::Number(Number::F64(n)));
        }

        self.position -= 1;

        if let lexer::Token::True = self.next_token()?.token() {
            return Ok(Expression::True);
        }

        self.position -= 1;

        if let lexer::Token::False = self.next_token()?.token() {
            return Ok(Expression::False);
        }

        self.position -= 1;

        if let lexer::Token::Integer(n) = self.next_token()?.token() {
            if let Ok(_) = self.expect(vec![lexer::Token::I64Type]) {
                return Ok(Expression::Number(Number::I64(n)));
            }
            if let Ok(_) = self.expect(vec![lexer::Token::I32Type]) {
                return Ok(Expression::Number(Number::I32(n as i32)));
            }
            if let Ok(_) = self.expect(vec![lexer::Token::I16Type]) {
                return Ok(Expression::Number(Number::I16(n as i16)));
            }
            if let Ok(_) = self.expect(vec![lexer::Token::I8Type]) {
                return Ok(Expression::Number(Number::I8(n as i8)));
            }
            return Ok(Expression::Number(Number::I32(n as i32)));
        }

        self.position -= 1;

        if let lexer::Token::Char(c) = self.next_token()?.token() {
            return Ok(Expression::Char(c));
        }

        self.position -= 1;

        if let lexer::Token::String(s) = self.next_token()?.token() {
            return Ok(Expression::String(s));
        }

        self.position -= 1;

        if let Ok(_) = self.expect(vec![lexer::Token::LeftParen]) {
            let expr = self.expr()?;
            self.consume(lexer::Token::RightParen)?;

            return Ok(Expression::Grouping(Box::new(expr)));
        }

        let tok = self.next_token()?;

        if let lexer::Token::Identifier(s) = tok.token() {
            if let Ok(_) = self.expect(vec![lexer::Token::LeftParen]) {
                self.position -= 1;
                let args = self.arguments()?;

                let mut function = None;
                for fun in self.functions.clone() {
                    if fun.id == s {
                        function = Some(fun);
                    }
                }

                if let Some(f) = function {
                    if f.parameters.len() != args.len() {
                        return Err(Error::ParserError {
                            at: tok.position(),
                            message: format!(
                                "This function takes {} arguments but {} passed",
                                f.parameters.len(),
                                args.len()
                            ),
                        });
                    }

                    for (index, p) in f.parameters.iter().enumerate() {
                        if p.ty != self.evaluate_expr_type(args.get(index).unwrap().clone())? {
                            return Err(Error::MismatchedTypes {
                                expected: p.ty.clone(),
                                actual: self
                                    .evaluate_expr_type(args.get(index).unwrap().clone())?,
                                at: tok.position(),
                            });
                        }
                    }

                    return Ok(Expression::Call {
                        id: s,
                        parameters: args,
                        ret_type: f.ret_type,
                    });
                } else {
                    return Err(Error::NoFn(s, tok.position()));
                }
            } else {
                return Ok(Expression::Identifier(s));
            }
        }

        self.position -= 1;

        let tok = self.next_token()?;

        self.position -= 1;

        Err(Error::ParserError {
            at: tok.position(),
            message: format!("Expected `primary` but found {:?}", tok.token()),
        })
    }

    pub fn process_functions(&mut self) -> Result<Vec<Function>, Error> {
        let mut functions = Vec::new();
        while let Ok(tok) = self.next_token() {
            if let lexer::Token::Fn = tok.token() {
                let id = self.identifier()?;
                self.consume(lexer::Token::LeftParen)?;

                let param = self.param();

                let params = if let Ok(param) = param {
                    let mut params = vec![param];

                    while let Ok(_) = self.expect(vec![lexer::Token::Comma]) {
                        params.push(self.param()?);
                    }

                    self.consume(lexer::Token::RightParen)?;

                    params
                } else {
                    self.consume(lexer::Token::RightParen)?;
                    vec![]
                };

                let mut ret_type = Type::Void;

                if let Ok(_) = self.expect(vec![lexer::Token::ReturnArrow]) {
                    ret_type = self.ty()?;
                }

                functions.push(Function {
                    parameters: params.clone(),
                    id: id.clone(),
                    ret_type: ret_type.clone(),
                });
            }

            if let lexer::Token::Asm = tok.token() {
                let id = self.identifier()?;
                self.consume(lexer::Token::LeftParen)?;

                let param = self.param();

                let params = if let Ok(param) = param {
                    let mut params = vec![param];

                    while let Ok(_) = self.expect(vec![lexer::Token::Comma]) {
                        params.push(self.param()?);
                    }

                    self.consume(lexer::Token::RightParen)?;

                    params
                } else {
                    self.consume(lexer::Token::RightParen)?;
                    vec![]
                };

                let mut ret_type = Type::Void;

                if let Ok(_) = self.expect(vec![lexer::Token::ReturnArrow]) {
                    ret_type = self.ty()?;
                }

                functions.push(Function {
                    parameters: params.clone(),
                    id: id.clone(),
                    ret_type: ret_type.clone(),
                });
            }
        }

        self.position = 0;

        Ok(functions)
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>, Error> {
        let mut statements = Vec::new();

        // Define functions first
        while let Ok(tok) = self.next_token() {
            if let lexer::Token::Use = tok.token() {
                if let lexer::Token::String(file) = self.next_token()?.token() {
                    let source = fs::read_to_string(file).unwrap();
                    let mut lexer = Lexer::new(&source);
                    let mut parser = Parser::new(&mut lexer);
                    let mut functions = parser.process_functions()?;
                    self.functions.append(&mut functions);
                } else {
                    return Err(Error::ParserError {
                        at: tok.position(),
                        message: format!("Expected String but found {:?}", tok.token()),
                    });
                }
            }
        }

        self.position = 0;

        let mut funcs = self.process_functions()?;

        self.functions.append(&mut funcs);

        loop {
            if self.position == self.tokens.len() {
                break;
            }

            match self.statement() {
                Ok(st) => statements.push(st),
                Err(Error::EndOfFile) => break,
                Err(e) => return Err(e),
            }
        }
        Ok(statements)
    }
}
