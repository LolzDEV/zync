const NOT_IDENTIFIER: [char; 15] = [
    '+', '-', '/', '*', '=', ';', '(', ')', '{', '}', ' ', ',', ':', '\"', '\'',
];

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Float(f64),
    Integer(i64),
    Plus,
    Minus,
    Star,
    Slash,
    SemiColon,
    Identifier(String),
    String(String),
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
    F64Type,
    F32Type,
    I64Type,
    I32Type,
    I16Type,
    I8Type,
    StringType,
    VoidType,
    Use,
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
        let mut current_string = String::new();
        let mut is_string = false;
        let mut pos = FilePosition { line: 0, column: 0 };

        for line in self.source.lines().into_iter().skip(self.current_line) {
            for c in line.chars().into_iter().skip(self.current) {
                pos = FilePosition {
                    line: self.current_line + 1,
                    column: self.current + 1,
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
                    '"' => {
                        if is_string {
                            return (Token::String(current_string), pos);
                        } else {
                            is_string = true;
                            continue;
                        }
                    }
                    _ => (),
                }

                if c.is_ascii_alphanumeric() && !is_string {
                    if (c.is_ascii_digit() && is_identifier)
                        || (!c.is_ascii_digit() && is_identifier)
                        || (!c.is_ascii_digit() && !is_identifier)
                    {
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
                                "f64" => return (Token::F64Type, pos),
                                "f32" => return (Token::F32Type, pos),
                                "i64" => return (Token::I64Type, pos),
                                "i32" => return (Token::I32Type, pos),
                                "i16" => return (Token::I16Type, pos),
                                "i8" => return (Token::I8Type, pos),
                                "string" => return (Token::StringType, pos),
                                "void" => return (Token::VoidType, pos),
                                "ret" => return (Token::Ret, pos),
                                "use" => return (Token::Use, pos),
                                _ => {
                                    return (Token::Identifier(current_identifier), pos);
                                }
                            }
                        }
                    }
                }

                if is_string {
                    current_string.push(c);
                    continue;
                }

                if c.is_ascii_whitespace() {
                    current_identifier = String::new();
                    continue;
                }

                if c.is_ascii_digit() || c == '.' {
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
                        && self
                            .source
                            .lines()
                            .into_iter()
                            .nth(self.current_line)
                            .unwrap_or(" ")
                            .chars()
                            .into_iter()
                            .nth(self.current)
                            .unwrap_or('c')
                            != '.'
                    {
                        if current_number.contains('.') {
                            if let Ok(n) = current_number.parse::<f64>() {
                                return (Token::Float(n), pos);
                            }
                        } else {
                            if let Ok(n) = current_number.parse::<i64>() {
                                return (Token::Integer(n), pos);
                            }
                        }
                    }
                }
            }

            self.current = 0;
            self.current_line += 1;
        }

        if is_number {
            if current_number.contains('.') {
                if let Ok(n) = current_number.parse::<f64>() {
                    return (Token::Float(n), pos);
                }
            } else {
                if let Ok(n) = current_number.parse::<i64>() {
                    return (Token::Integer(n), pos);
                }
            }
        }

        (Token::Eof, pos)
    }
}
