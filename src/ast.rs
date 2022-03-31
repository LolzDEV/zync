use crate::lexer::{Lexer, Token, FilePosition};

#[derive(Debug, Clone)]
pub enum Op {
    Plus,
    Minus,
    Star,
    Slash,
    Assign,
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
    },
    Block(Vec<AstNode>),
    Parameters(Vec<AstNode>),
    ExprStatement(Box<AstNode>),
    LetStatement(Box<AstNode>),
    Grouping(Box<AstNode>),
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
            .clone().0
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

    pub fn statement(&mut self) -> Result<AstNode, Error> {
        if let Ok(_) = self.expect(vec![Token::Let]) {
            return self.let_statement();
        }

        self.expr_statement()
    }

    pub fn expr_statement(&mut self) -> Result<AstNode, Error> {
        let stmt = Ok(AstNode::ExprStatement(Box::new(self.expr()?)));
        self.consume(Token::SemiColon)?;
        stmt
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
            lhs = AstNode::BinaryExpr { lhs: Box::new(lhs), op: operator, rhs: Box::new(rhs) }
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
            return Ok(AstNode::Identifier(s));
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
}
