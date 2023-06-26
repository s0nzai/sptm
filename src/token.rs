
use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Tag {
    LParen,
    RParen,
    Semicolon,
    Comma,
    Period,
    Not,
    And,
    Program,
    Symbol,
    Proc,
    Begin,
    End,
    If,
    Then,
    Elsif,
    Else,
    While,
    Do,
    Repeat,
    Until,
    Or,
    Print,
    Erase,
    Left,
    Right,
    Blank,
    Sym(char),
    Ident(String),
    Eof,
}

impl fmt::Display for Tag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let v;
        let s = match self {
            Tag::LParen => "<(>",
            Tag::RParen => "<)>",
            Tag::Semicolon => "<;>",
            Tag::Comma => "<,>",
            Tag::Period => "<.>",
            Tag::Not => "<~>",
            Tag::And => "<&>",
            Tag::Program => "<program>",
            Tag::Symbol => "<symbol>",
            Tag::Proc => "<proc>",
            Tag::Begin => "<begin>",
            Tag::End => "<end>",
            Tag::If => "<if>",
            Tag::Then => "<then>",
            Tag::Elsif => "<elsif>",
            Tag::Else => "<else>",
            Tag::While => "<while>",
            Tag::Do => "<do>",
            Tag::Repeat => "<repeat>",
            Tag::Until => "<until>",
            Tag::Or => "<or>",
            Tag::Print => "<print>",
            Tag::Erase => "<erase>",
            Tag::Left => "<left>",
            Tag::Right => "<right>",
            Tag::Blank => "<blank>",
            Tag::Sym(s) => {
                v = format!("<sym: {}>", s);
                &v
            },
            Tag::Ident(id) => {
                v = format!("<ident: {}>", id);
                &v
            }
            Tag::Eof => "<EOF>", 
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub line: u32,
    pub pos: u32,
    pub tag: Tag,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{}:{}> {}", self.line, self.pos, self.tag)
    }
}

