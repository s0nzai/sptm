use crate::token::Tag;
use std::fmt;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Error {
    line: u32,
    pos: u32,
    kind: ErrorKind,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{} {}", self.line, self.pos, self.kind)
    }
}

impl Error {
    pub fn new(line: u32, pos: u32, kind: ErrorKind) -> Self {
        Self {
            line,
            pos,
            kind,
        }
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    UnexpectedChar(char),
    UnexpectedToken(Tag),
    ProcNameMatch(String),
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::UnexpectedChar(c) => {
                write!(f, "Unexpected charactor {}", c)
            },
            Self::UnexpectedToken(t) => {
                write!(f, "Unexpected token {}", t)
            },
            Self::ProcNameMatch(id) => {
                write!(f, "procedure name {} does not match", id)
            }
        }
    }
}

