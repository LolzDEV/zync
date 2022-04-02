const NOT_IDENTIFIER: [char; 13] = ['+', '-', '/', '*', '=', ';', '(', ')', '{', '}', ' ', ',', ':'];

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Number(f64),
    Plus,
    Minus,
    Star,
    Slash,
    SemiColon,
    Identifier(String),
    LeftParen,
    RightParen,
    Assign,
    LeftBracket,
    RightBracket,
    Let,
    Fn,
    ReturnArrow,
    Comma,
    Colon,
    Ret,
    NumberType,
    VoidType,
    Eof,
}

pub struct Lexer {
    source: String,
    current: usize,
    current_line: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct FilePosition {
    pub line: usize,
    pub column: usize,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.to_string(),
            current: 0,
            current_line: 0,
        }
    }

    pub fn match_char(&self, ch: char) -> bool {
        if self
            .source
            .lines()
            .into_iter()
            .nth(self.current_line)
            .unwrap_or(" ")
            .chars()
            .into_iter()
            .nth(self.current)
            .unwrap_or(' ')
            == ch
        {
            true
        } else {
            false
        }
    }

    pub fn next(&mut self) -> (Token, FilePosition) {
        let mut is_number = false;
        let mut current_number = String::new();
        let mut current_identifier = String::new();
        let mut is_identifier = false;
        let mut pos = FilePosition { line: 0, column: 0 };

        for (l, line) in self
            .source
            .lines()
            .into_iter()
            .skip(self.current_line)
            .enumerate()
        {
            for (ch, c) in line.chars().into_iter().skip(self.current).enumerate() {
                pos = FilePosition {
                    line: l,
                    column: ch,
                };

                self.current += 1;

                match c {
                    '+' => return (Token::Plus, pos),
                    '-' => {
                        if self.match_char('>') {
                            self.current += 1;
                            return (Token::ReturnArrow, pos);
                        } else {
                            return (Token::Minus, pos);
                        }
                    }
                    '*' => return (Token::Star, pos),
                    '/' => return (Token::Slash, pos),
                    ';' => return (Token::SemiColon, pos),
                    '=' => return (Token::Assign, pos),
                    '(' => return (Token::LeftParen, pos),
                    ')' => return (Token::RightParen, pos),
                    '{' => return (Token::LeftBracket, pos),
                    '}' => return (Token::RightBracket, pos),
                    ',' => return (Token::Comma, pos),
                    ':' => return (Token::Colon, pos),
                    _ => (),
                }

                if c.is_ascii_whitespace() {
                    current_identifier = String::new();
                    continue;
                }

                if c.is_ascii_digit() {
                    if !is_number {
                        is_number = true;
                    }

                    current_number.push(c);

                    if !self
                        .source
                        .lines()
                        .into_iter()
                        .nth(self.current_line)
                        .unwrap_or(" ")
                        .chars()
                        .into_iter()
                        .nth(self.current)
                        .unwrap_or('c')
                        .is_ascii_digit()
                    {
                        if let Ok(n) = current_number.parse::<f64>() {
                            return (Token::Number(n), pos);
                        }
                    }
                }

                if c.is_ascii_alphanumeric() {
                    if !is_identifier {
                        is_identifier = true;
                    }

                    current_identifier.push(c);

                    let next = self
                        .source
                        .lines()
                        .into_iter()
                        .nth(self.current_line)
                        .unwrap_or(" ")
                        .chars()
                        .into_iter()
                        .nth(self.current)
                        .unwrap_or(' ');

                    if NOT_IDENTIFIER.contains(&next) {
                        match current_identifier.as_str() {
                            "let" => return (Token::Let, pos),
                            "fn" => return (Token::Fn, pos),
                            "number" => return (Token::NumberType, pos),
                            "void" => return (Token::VoidType, pos),
                            "ret" => return (Token::Ret, pos), 
                            _ => {
                                return (Token::Identifier(current_identifier), pos);
                            }
                        }
                    }
                }
            }

            self.current = 0;
            self.current_line += 1;
        }

        if is_number {
            if let Ok(n) = current_number.parse::<f64>() {
                return (Token::Number(n), pos);
            }
        }

        (Token::Eof, pos)
    }
}
