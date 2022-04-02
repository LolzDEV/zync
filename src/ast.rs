use inkwell::{
    context::Context,
    types::{BasicMetadataTypeEnum, FunctionType},
};

use crate::lexer::{FilePosition, Lexer, Token};

#[derive(Debug, Clone)]
pub enum Op {
    Plus,
    Minus,
    Star,
    Slash,
    Assign,
}

#[derive(Debug, Clone)]
pub enum Type {
    Number,
    Void,
}

impl Type {
    pub fn to_function_type<'a>(
        &self,
        context: &'a Context,
        params: &[BasicMetadataTypeEnum<'a>],
    ) -> FunctionType<'a> {
        match self {
            Type::Number => context.f64_type().fn_type(params, false),
            Type::Void => context.void_type().fn_type(params, false),
        }
    }

    pub fn to_type<'a>(&self, context: &'a Context) -> Option<BasicMetadataTypeEnum<'a>> {
        match self {
            Type::Number => Some(context.f64_type().into()),
            Type::Void => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum AstNode {
    Number(f64),
    BinaryExpr {
        lhs: Box<AstNode>,
        op: Token,
        rhs: Box<AstNode>,
    },
    Identifier(String),
    UnaryExpr {
        op: Token,
        rhs: Box<AstNode>,
    },
    Call {
        id: Box<AstNode>,
        args: Box<AstNode>,
    },
    Arguments(Vec<AstNode>),
    Definition {
        id: Box<AstNode>,
        parameters: Box<AstNode>,
        block: Box<AstNode>,
    },
    RetStatement(Box<AstNode>),
    Block(Vec<AstNode>),
    Parameter(Box<AstNode>, Type),
    Parameters(Vec<AstNode>, Type),
    ExprStatement(Box<AstNode>),
    LetStatement(Box<AstNode>),
    Grouping(Box<AstNode>),
    Type(Type),
    None,
}

#[derive(Debug, Clone, Copy)]
pub enum Error {
    UnexpectedToken,
    EndOfFile,
}

pub struct Parser {
    pub tokens: Vec<(Token, FilePosition)>,
    pub position: usize,
}

impl Parser {
    pub fn new(lexer: &mut Lexer) -> Self {
        let mut tokens = Vec::new();
        loop {
            let tok = lexer.next();
            if let (Token::Eof, _) = tok {
                break;
            }
            tokens.push(tok);
        }

        Self {
            tokens,
            position: 0,
        }
    }

    pub fn next_token(&mut self) -> Result<Token, Error> {
        let (tok, _) = self
            .tokens
            .get(self.position)
            .ok_or(Error::EndOfFile)?
            .clone();
        self.position += 1;
        Ok(tok)
    }

    pub fn check(&mut self, token: Token) -> Result<(), Error> {
        if self
            .tokens
            .get(self.position)
            .ok_or(Error::EndOfFile)?
            .clone()
            .0
            == token
        {
            Ok(())
        } else {
            Err(Error::UnexpectedToken)
        }
    }

    pub fn expect(&mut self, tokens: Vec<Token>) -> Result<(), Error> {
        for tok in tokens.iter() {
            if let Ok(_) = self.check(tok.clone()) {
                self.position += 1;
                return Ok(());
            }
        }

        Err(Error::UnexpectedToken)
    }

    pub fn previous(&mut self) -> Result<Token, Error> {
        let (tok, _) = self
            .tokens
            .get(self.position - 1)
            .ok_or(Error::EndOfFile)?
            .clone();
        Ok(tok)
    }

    pub fn consume(&mut self, token: Token) -> Result<(), Error> {
        if let Ok(_) = self.check(token) {
            self.position += 1;
            Ok(())
        } else {
            Err(Error::UnexpectedToken)
        }
    }

    pub fn block(&mut self) -> Result<AstNode, Error> {
        self.consume(Token::LeftBracket)?;

        let mut statements = Vec::new();

        while let Err(_) = self.expect(vec![Token::RightBracket]) {
            statements.push(self.statement()?);
        }

        Ok(AstNode::Block(statements))
    }

    pub fn statement(&mut self) -> Result<AstNode, Error> {
        if let Ok(_) = self.expect(vec![Token::Fn]) {
            return self.fn_statement();
        }

        if let Ok(_) = self.expect(vec![Token::Let]) {
            return self.let_statement();
        }

        if let Ok(_) = self.expect(vec![Token::Ret]) {
            return self.ret_statement();
        }

        self.expr_statement()
    }

    pub fn ret_statement(&mut self) -> Result<AstNode, Error> {
        let stmt = Ok(AstNode::RetStatement(Box::new(self.expr()?)));
        self.consume(Token::SemiColon)?;
        stmt
    }

    pub fn expr_statement(&mut self) -> Result<AstNode, Error> {
        let stmt = Ok(AstNode::ExprStatement(Box::new(self.expr()?)));
        self.consume(Token::SemiColon)?;
        stmt
    }

    pub fn parameters(&mut self) -> Result<AstNode, Error> {
        self.consume(Token::LeftParen)?;

        let param = self.param();

        let params = if let Ok(param) = param {
            let mut params = vec![param];

            while let Ok(_) = self.expect(vec![Token::Comma]) {
                params.push(self.param()?);
            }

            self.consume(Token::RightParen)?;

            params
        } else {
            self.consume(Token::RightParen)?;
            vec![]
        };

        self.consume(Token::ReturnArrow)?;

        let ret = self.ty()?;

        Ok(AstNode::Parameters(params, ret))
    }

    pub fn arguments(&mut self) -> Result<AstNode, Error> {
        let arg = self.expr();

        if let Ok(arg) = arg {
            let mut args = vec![arg];

            while let Ok(_) = self.expect(vec![Token::Comma]) {
                args.push(self.expr()?);
            }

            Ok(AstNode::Arguments(args))
        } else {
            Ok(AstNode::Arguments(vec![]))
        }
    }

    pub fn fn_statement(&mut self) -> Result<AstNode, Error> {
        let id = self.identifier()?;
        let params = self.parameters()?;
        if let Ok(_) = self.expect(vec![Token::SemiColon]) {
            Ok(AstNode::Definition {
                id: Box::new(id),
                parameters: Box::new(params),
                block: Box::new(AstNode::None),
            })
        } else {
            Ok(AstNode::Definition {
                id: Box::new(id),
                parameters: Box::new(params),
                block: Box::new(self.block()?),
            })
        }
    }

    pub fn let_statement(&mut self) -> Result<AstNode, Error> {
        let stmt = Ok(AstNode::LetStatement(Box::new(self.expr()?)));
        self.consume(Token::SemiColon)?;
        stmt
    }

    pub fn expr(&mut self) -> Result<AstNode, Error> {
        self.assignment()
    }

    pub fn assignment(&mut self) -> Result<AstNode, Error> {
        let mut lhs = self.term()?;

        while let Ok(_) = self.expect(vec![Token::Assign]) {
            let operator = self.previous()?;

            let rhs = self.term()?;
            lhs = AstNode::BinaryExpr {
                lhs: Box::new(lhs),
                op: operator,
                rhs: Box::new(rhs),
            }
        }

        Ok(lhs)
    }

    pub fn term(&mut self) -> Result<AstNode, Error> {
        let mut lhs = self.factor()?;

        while let Ok(_) = self.expect(vec![Token::Minus, Token::Plus]) {
            let operator = self.previous()?;

            let rhs = self.factor()?;
            lhs = AstNode::BinaryExpr {
                lhs: Box::new(lhs),
                op: operator,
                rhs: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    pub fn factor(&mut self) -> Result<AstNode, Error> {
        let mut expr = self.unary()?;

        while let Ok(_) = self.expect(vec![Token::Star, Token::Slash]) {
            let operator = self.previous()?;
            let rhs = self.unary()?;
            expr = AstNode::BinaryExpr {
                lhs: Box::new(expr),
                op: operator,
                rhs: Box::new(rhs),
            }
        }

        Ok(expr)
    }

    pub fn unary(&mut self) -> Result<AstNode, Error> {
        if let Ok(_) = self.expect(vec![Token::Minus]) {
            let op = self.previous()?;
            let rhs = self.unary()?;

            return Ok(AstNode::UnaryExpr {
                op,
                rhs: Box::new(rhs),
            });
        }

        self.primary()
    }

    pub fn primary(&mut self) -> Result<AstNode, Error> {
        if let Token::Number(n) = self.next_token()? {
            return Ok(AstNode::Number(n));
        }

        self.position -= 1;

        if let Token::Identifier(s) = self.next_token()? {
            if let Ok(_) = self.expect(vec![Token::LeftParen]) {
                let args = self.arguments()?;
                self.consume(Token::RightParen)?;
                return Ok(AstNode::Call {
                    id: Box::new(AstNode::Identifier(s)),
                    args: Box::new(args),
                });
            } else {
                return Ok(AstNode::Identifier(s));
            }
        }

        self.position -= 1;

        if let Ok(_) = self.expect(vec![Token::LeftParen]) {
            let expr = self.expr()?;
            self.consume(Token::RightParen)?;

            return Ok(AstNode::Grouping(Box::new(expr)));
        }

        Err(Error::UnexpectedToken)
    }

    pub fn parse(&mut self) -> Result<Vec<AstNode>, Error> {
        let mut statements = Vec::new();

        loop {
            match self.statement() {
                Ok(st) => statements.push(st),
                Err(Error::EndOfFile) => break,
                Err(e) => return Err(e),
            }
        }
        Ok(statements)
    }

    pub fn ty(&mut self) -> Result<Type, Error> {
        if let Token::NumberType = self.next_token()? {
            return Ok(Type::Number);
        }

        if let Token::VoidType = self.next_token()? {
            return Ok(Type::Void);
        }

        self.position -= 1;

        Err(Error::UnexpectedToken)
    }

    pub fn identifier(&mut self) -> Result<AstNode, Error> {
        if let Token::Identifier(s) = self.next_token()? {
            return Ok(AstNode::Identifier(s));
        } else {
            self.position -= 1;
        }

        Err(Error::UnexpectedToken)
    }

    pub fn param(&mut self) -> Result<AstNode, Error> {
        let param = self.identifier()?;
        self.consume(Token::Colon)?;
        let ty = self.ty()?;
        Ok(AstNode::Parameter(Box::new(param), ty))
    }
}
