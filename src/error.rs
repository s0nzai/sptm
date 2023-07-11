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
    DupSymbol(char),
    DupIdent(String),
    NotFound(String),
    IsProc(String),
    IsSym(String),
    ArgsNum(String),
    NotSym(char),
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
                write!(f, "Procedure name {} does not match", id)
            },
            Self::DupSymbol(s) => {
                write!(f, "Duplicate symbol {}", s)
            },
            Self::DupIdent(id) => {
                write!(f, "Duplicate ident {}", id)
            },
            Self::NotFound(id) => {
                write!(f, "Ident {} is not found", id)
            },
            Self::IsProc(id) => {
                write!(f, "Ident {} is procedure", id)
            },
            Self::IsSym(id) => {
                write!(f, "Ident {} is symbol", id)
            },
            Self::ArgsNum(id) => {
                write!(f, "Number of args of {} is different.", id)
            },
            Self::NotSym(s) => {
                write!(f, "{} is not in symbols", s)
            },
        }
    }
}

