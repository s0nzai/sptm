use std::fmt;
use std::collections::HashMap;
use crate::token::{Tag, Token};

pub struct Lexer {
    pub line: u32,
    pub pos: u32,
    cur: char,
    buf: Vec<char>,
    buf_idx: usize,
}

#[derive(Debug)]
enum ErrorKind {
    Unexpected,
}

#[derive(Debug)]
pub struct Error {
    line: u32,
    pos: u32,
    kind: ErrorKind,
    cur: char,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            ErrorKind::Unexpected => {
                if self.cur == '\0' {
                    write!(f, "{}:{} Unexpected EOF", self.line, self.pos)
                } else {
                    write!(f, "{}:{} Unexpected character '{}'", self.line, self.pos, self.cur)
                }
            },
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

impl Lexer {
    pub fn new(input: String) -> Lexer {
        Lexer {
            line: 1,
            pos: 1,
            cur: '\0',
            buf: input.chars().collect(),
            buf_idx: 0,
        }
    }

    fn read(&mut self) {
        if let Some(c) = self.buf.get(self.buf_idx) {
            self.pos += 1;
            self.cur = *c;
            self.buf_idx += 1;
        } else {
            self.pos += 1;
            self.cur = '\0';
        }
    }

    fn token(&self, tag: Tag) -> Token {
        Token {
            line: self.line,
            pos: self.pos,
            tag,
        }
    }

    fn error(&self, kind: ErrorKind) -> Error {
        Error {
            line: self.line,
            pos: self.pos,
            kind,
            cur: self.cur,
        }
    }

    fn get_string(&mut self) -> String {
        let mut s = String::new();
        while self.cur.is_ascii_alphanumeric() || self.cur == '_' {
            s.push(self.cur);
            self.read(); 
        }
        s
    }

    fn ident(&mut self) -> Token {
        let keywords = [
            ("program", Tag::Program),
            ("symbol", Tag::Symbol),
            ("proc", Tag::Proc),
            ("begin", Tag::Begin),
            ("end", Tag::End),
            ("if", Tag::If),
            ("then", Tag::Then),
            ("elsif", Tag::Elsif),
            ("else", Tag::Else),
            ("while", Tag::While),
            ("do", Tag::Do),
            ("repeat", Tag::Repeat),
            ("until", Tag::Until),
            ("or", Tag::Or),
            ("print", Tag::Print),
            ("erase", Tag::Erase),
            ("left", Tag::Left),
            ("right", Tag::Right),
            ("blank", Tag::Blank),
        ];
        let line = self.line;
        let pos = self.pos;
        let map_kw = HashMap::from(keywords);
        let s = self.get_string();
        if let Some(t) = map_kw.get(s.as_str()) {
            Token {
                line,
                pos,
                tag: t.clone(),
            }
        } else {
            Token {
                line,
                pos,
                tag: Tag::Ident(s.clone())
            }
        }
    }

    fn punct(&mut self) -> Result<Token> {
        let t = match self.cur {
             '(' => Some(Tag::LParen),
             ')' => Some(Tag::RParen),
             ';' => Some(Tag::Semicolon),
             ',' => Some(Tag::Comma),
             '.' => Some(Tag::Period),
             '~' => Some(Tag::Not),
             '&' => Some(Tag::And),
             _ => None,
        };
        if let Some(t) = t {
            let punct = self.token(t);
            self.read();
            Ok(punct)
        } else {
            if self.cur.is_ascii_alphabetic() || self.cur == '_' {
                Ok(self.ident())
            } else if self.cur.is_ascii_digit() || self.cur.is_ascii_punctuation() {
                let sym = self.token(Tag::Sym(self.cur));
                self.read();
                Ok(sym)
            } else {
                Err(self.error(ErrorKind::Unexpected))
            }
        }
    }

    fn comment(&mut self) -> Result<()> {
        self.read();
        while self.cur != '\0' {
            if self.cur == '(' {
                self.read();
                if self.cur == '*' {
                    self.comment()?;
                }
            } else if self.cur == '*' {
                self.read();
                if self.cur == ')' {
                    self.read();
                    break;
                }
            } else {
                self.read();
            }
        }
        if self.cur == '\0' {
            Err(self.error(ErrorKind::Unexpected))
        } else {
            Ok(())
        }
    }

    pub fn lexing(&mut self) -> Result<Vec<Token>> {
        let mut stream = Vec::new();
        self.read();
        while self.cur != '\0' {
            match self.cur {
                ' ' | '\t' => {
                    self.read();
                },
                '\n' => {
                    self.line += 1;
                    self.pos = 0;
                    self.read();
                }
                '\"' => {
                    self.read();
                    let c = self.cur;
                    if c.is_control() {
                        return Err(self.error(ErrorKind::Unexpected));
                    }
                    self.read();
                    if self.cur != '\"' {
                        return Err(self.error(ErrorKind::Unexpected));
                    }
                    self.read();
                    stream.push(self.token(Tag::Sym(c)));
                },
                '(' => {
                    let lparen = self.token(Tag::LParen);
                    self.read();
                    if self.cur == '*' {
                        self.comment()?;
                    } else {
                        stream.push(lparen);
                    }
                },
                '\0' => {
                    stream.push(self.token(Tag::Eof));
                }
                _ => {
                    let t = self.punct()?;
                    stream.push(t); 
                }
            }
        }
        Ok(stream)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn ident() {
        let test_file = "test_ident1234\n".to_string();
        let mut l  = Lexer::new(test_file);
        assert_eq!(l.lexing().unwrap()[0].tag, Tag::Ident("test_ident1234".to_string()))
    }

    #[test]
    fn comment() {
        let test_file = "left (* This is (*a*) comment.*) right".to_string();
        let mut l = Lexer::new(test_file);
        let mut v = Vec::new();
        for t in l.lexing().unwrap() {
            v.push(t.tag);
        }
        let ans = vec![
            Tag::Left,
            Tag::Right,
        ];
        assert_eq!(v, ans)
    }

    #[test]
    fn sym() {
        let test_file = "\"a\"".to_string();
        let mut l = Lexer::new(test_file);
        assert_eq!(l.lexing().unwrap()[0].tag, Tag::Sym('a'))
    }

    #[test]
    fn keywords() {
        let test_file =
        "program symbol proc begin end
        if then elsif else while do repeat until
            or print erase left right blank".to_string();
        let mut l = Lexer::new(test_file);
        let mut v = Vec::new();
        for t in l.lexing().unwrap() {
            v.push(t.tag);
        }
        let ans = vec![
            Tag::Program,
            Tag::Symbol,
            Tag::Proc,
            Tag::Begin,
            Tag::End,
            Tag::If,
            Tag::Then,
            Tag::Elsif,
            Tag::Else,
            Tag::While,
            Tag::Do,
            Tag::Repeat,
            Tag::Until,
            Tag::Or,
            Tag::Print,
            Tag::Erase,
            Tag::Left,
            Tag::Right,
            Tag::Blank,
        ];
        assert_eq!(v, ans)
    }

    #[test]
    fn punct() {
        let test_file = "();,.~&".to_string();
        let mut l = Lexer::new(test_file);
        let mut v = Vec::new();
        for t in l.lexing().unwrap() {
            v.push(t.tag);
        }
        let ans = vec![
            Tag::LParen,
            Tag::RParen,
            Tag::Semicolon,
            Tag::Comma,
            Tag::Period,
            Tag::Not,
            Tag::And,
        ];
        assert_eq!(v, ans)
    }
}

