#[derive(Debug)]
pub enum Token {
    Number(f64),
    Plus,
    Minus,
    Star,
    Slash,
    SemiColon,
    Identifier(String),
    Assign,
    Let,
    Eof,
}

pub struct Lexer {
    source: String,
    current: usize,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.to_string(),
            current: 0,
        }
    }

    pub fn next(&mut self) -> Token {
        let mut is_number = false;
        let mut current_number = String::new();
        let mut current_identifier = String::new();
        let mut is_identifier = false;

        for c in self.source.chars().into_iter().skip(self.current) {
            self.current += 1;

            match c {
                '+' => return Token::Plus,
                '-' => return Token::Minus,
                '*' => return Token::Star,
                '/' => return Token::Slash,
                ';' => return Token::SemiColon,
                '=' => return Token::Assign,
                _ => (),
            }

            if c.is_ascii_whitespace() {
                if is_identifier {
                    return Token::Identifier(current_identifier)
                }

                continue;
            }

            if c.is_ascii_digit() {
                if !is_number {
                    is_number = true;
                }

                current_number.push(c);

                if !self
                    .source
                    .chars()
                    .into_iter()
                    .nth(self.current)
                    .unwrap_or('c')
                    .is_ascii_digit()
                {
                    if let Ok(n) = current_number.parse::<f64>() {
                        return Token::Number(n);
                    }
                }
            }

            if c.is_ascii_alphanumeric() {
                if !is_identifier {
                    is_identifier = true;
                }

                current_identifier.push(c);

                if self
                    .source
                    .chars()
                    .into_iter()
                    .nth(self.current)
                    .unwrap_or(' ')
                    .is_ascii_whitespace()
                {
                    match current_identifier.as_str() {
                        "let" => return Token::Let,
                        _ => ()
                    }
                } 
            }
        }

        if is_number {
            if let Ok(n) = current_number.parse::<f64>() {
                return Token::Number(n);
            }
        }

        Token::Eof
    }
}
