
use std::fmt;

#[derive(PartialEq, Debug, Clone)]
pub enum Val {
    Sym(Node<char>),
    Var(Node<String>),
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Val::Sym(s) => write!(f, "{}", s.node),
            Val::Var(v) => write!(f, "{}", v.node),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    Or(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Not(Box<Expr>),
    Val(Val),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Stat {
    Call(Node<String>, Vec<Val>),
    If(Expr, Vec<Stat>, Vec<Stat>),
    While(Expr, Vec<Stat>),
    Repeat(Vec<Stat>, Expr),
    Print(Val),
    Left,
    Right,
    Erase,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Proc {
    pub proc_name: Node<String>,
    pub args: Vec<Node<String>>,
    pub symbols: Vec<char>,
    pub proc_list: Vec<Proc>,
    pub stat_list: Vec<Stat>,
}

#[derive(Debug, Clone)]
pub struct Node<T: PartialEq> {
    pub line: u32,
    pub pos: u32,
    pub node: T,
}

impl<T: PartialEq> PartialEq for Node<T> {
    fn eq(&self, other: &Self) -> bool {
        self.node == other.node
    }
}

