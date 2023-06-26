
use std::fmt;
use crate::token::{Token, Tag};
use crate::tree::*;

#[derive(Debug)]
pub enum Error {
    Unexpected(Token),
    ProcNameMatch(Token),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Unexpected(t) => write!(f, "{}:{} Unexpected token {}", t.line, t.pos, t.tag),
            Self::ProcNameMatch(t) => write!(f, "{}:{} Procedure name {} does not match", t.line, t.pos, t.tag),
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

pub struct Parser {
    stream: Vec<Token>,
    stream_idx: usize,
    cur: Token,
}

impl Parser {
    pub fn new(stream: Vec<Token>) -> Self {
        let cur = Token {
            pos: 0,
            line: 0,
            tag: Tag::Eof,
        };
        Self {
            stream,
            stream_idx: 0,
            cur,
        }
    }

    fn read(&mut self) {
        match self.stream.get(self.stream_idx) {
            Some(t) => {
                self.cur = t.clone();
                self.stream_idx += 1;
            },
            None => {
                let t = Token {
                    pos: self.cur.pos,
                    line: self.cur.line,
                    tag: Tag::Eof,
                };
                self.cur = t;
            }
        }
    }

    fn expect(&mut self, t: Tag) -> Result<()> {
        if self.cur.tag == t {
            self.read();
            Ok(())
        } else {
            Err(Error::Unexpected(self.cur.clone()))
        }
    }

    fn get_node<T: PartialEq>(&self, node: T) -> Node<T> {
        Node {
            line: self.cur.line,
            pos: self.cur.pos,
            node,
        }
    }

    fn get_sym(&mut self) -> Result<char> {
        match self.cur.tag {
            Tag::Sym(s) => {
                self.read();
                Ok(s)
            },
            Tag::Blank => {
                self.read();
                Ok(' ')
            },
            _ => {
                Err(Error::Unexpected(self.cur.clone()))
            }
        }
    }

    fn get_ident(&mut self) -> Result<Node<String>> {
        let cur = self.cur.clone();
        match cur.tag {
            Tag::Ident(id) => {
                self.read();
                Ok(self.get_node(id))
            },
            _ => Err(Error::Unexpected(cur)),
        }
    }

    // Val = ident | sym.
    fn val(&mut self) -> Result<Val> {
        match self.cur.tag {
            Tag::Sym(s) => {
                let node = self.get_node(s);
                self.read();
                Ok(Val::Sym(node))
            },
            Tag::Ident(ref v) => {
                let node = self.get_node(v.clone());
                self.read();
                Ok(Val::Var(node))
            },
            Tag::Blank => {
                let node = self.get_node(' ');
                self.read();
                Ok(Val::Sym(node))
            },
            _ => Err(Error::Unexpected(self.cur.clone()))
        }
    }

    // Factor = Val | "(" Expr ")" | "~" Factor.
    fn factor(&mut self) -> Result<Expr> {
        match self.cur.tag {
            Tag::LParen => {
                self.read();
                let e = self.expr()?;
                self.expect(Tag::RParen)?;
                Ok(e)
            },
            Tag::Not => {
                self.read();
                let e = self.factor()?;
                Ok(Expr::Not(Box::new(e)))
            },
            _ => {
                let v = self.val()?;
                Ok(Expr::Val(v))
            }
        }
    }

    // Term = Factor {"&" Factor}.
    fn term(&mut self) -> Result<Expr> {
        let mut lhs = self.factor()?;
        while self.cur.tag == Tag::And {
            self.read();
            let rhs = self.factor()?;
            lhs = Expr::And(Box::new(lhs), Box::new(rhs));
        }
        Ok(lhs)
    }

    // Expr = Term {"or" Term}.
    fn expr(&mut self) -> Result<Expr> {
        let mut lhs = self.term()?;
        while self.cur.tag == Tag::Or {
            self.read();
            let rhs = self.term()?;
            lhs = Expr::Or(Box::new(lhs), Box::new(rhs));
        }
        Ok(lhs)
    }

    // RepeatStat = "repeat" StatList "until" Expr.
    fn repeat_stat(&mut self) -> Result<Stat> {
        self.expect(Tag::Repeat)?;
        let stat_list = self.stat_list()?;
        self.expect(Tag::Until)?;
        let e = self.expr()?;
        Ok(Stat::Repeat(stat_list, e))
    }

    // WhileStat = "while" Expr "do" StatList "end".
    fn while_stat(&mut self) -> Result<Stat> {
        self.expect(Tag::While)?;
        let e = self.expr()?;
        self.expect(Tag::Do)?;
        let stat_list = self.stat_list()?;
        self.expect(Tag::End)?;
        Ok(Stat::While(e, stat_list))
    }

    // IfStat = "if" Expr "then" StatList
    // {"elsif" Expr "then" StatList}
    // ["else" StatList] "end".
    fn if_stat(&mut self) -> Result<Stat> {
        let mut if_chain = Vec::new();
        self.expect(Tag::If)?;
        let e = self.expr()?;
        self.expect(Tag::Then)?;
        let stat_list = self.stat_list()?;
        while self.cur.tag == Tag::Elsif {
            self.read();
            let e = self.expr()?;
            self.expect(Tag::Then)?;
            let stat_list = self.stat_list()?;
            if_chain.push((e, stat_list));
        }
        let mut else_stat = if self.cur.tag == Tag::Else {
            self.read();
            let stat_list = self.stat_list()?;
            stat_list
        } else {
            Vec::new()
        };
        self.expect(Tag::End)?;
        for (e, stat_list) in if_chain.into_iter().rev() {
            else_stat = vec![Stat::If(e, stat_list, else_stat)];
        }
        Ok(Stat::If(e, stat_list, else_stat))
    }

    // ActualArgs = ["(" Val {"," Val} ")"].
    fn actual_args(&mut self) -> Result<Vec<Val>> {
        let mut args = Vec::new();
        if self.cur.tag == Tag::LParen {
            self.read();
            let v = self.val()?;
            args.push(v);
            while self.cur.tag == Tag::Comma {
                self.read();
                let v = self.val()?;
                args.push(v);
            }
            self.expect(Tag::RParen)?;
        }
        Ok(args)
    }

    // Call = ident ActualArgs.
    fn call(&mut self) -> Result<Stat> {
        let id = self.get_ident()?;
        let args = self.actual_args()?;
        Ok(Stat::Call(id, args))
    }

    // Inst = "print" "(" Val ")" | "left" | "right" | "erase".
    fn inst(&mut self) -> Result<Stat> {
        match self.cur.tag {
            Tag::Print => {
                self.read();
                self.expect(Tag::LParen)?;
                let v = self.val()?;
                self.expect(Tag::RParen)?;
                Ok(Stat::Print(v))
            },
            Tag::Left => {
                self.read();
                Ok(Stat::Left)
            },
            Tag::Right => {
                self.read();
                Ok(Stat::Right)
            },
            Tag::Erase => {
                self.read();
                Ok(Stat::Erase)
            },
            _ => Err(Error::Unexpected(self.cur.clone()))
        }
    }

    // Stat = [Call | IfStat | WhileStat | RepeatStat].
    fn stat(&mut self, stat_list: &mut Vec<Stat>) -> Result<()> {
        match self.cur.tag {
            Tag::If => stat_list.push(self.if_stat()?),
            Tag::While => stat_list.push(self.while_stat()?),
            Tag::Repeat => stat_list.push(self.repeat_stat()?),
            Tag::Print | Tag::Left | Tag::Right | Tag::Erase => stat_list.push(self.inst()?),
            Tag::Ident(_) => stat_list.push(self.call()?),
            _ => (),
        }
        Ok(())
    }

    // StatList = Stat {";" Stat}.
    fn stat_list(&mut self) -> Result<Vec<Stat>> {
        let mut stat_list = Vec::new();
        self.stat(&mut stat_list)?;
        while self.cur.tag == Tag::Semicolon {
            self.read();
            self.stat(&mut stat_list)?;
        }
        Ok(stat_list)
    }

    // ProcBody = Decls "begin" StatList "end".
    fn proc_body(&mut self, proc_list: &mut Vec<Proc>) -> Result<Vec<Stat>> {
        let mut decls = self.decls()?;
        proc_list.append(&mut decls);
        self.expect(Tag::Begin)?;
        let stat_list = self.stat_list()?;
        self.expect(Tag::End)?;
        Ok(stat_list)
    }

    // FormalArgs = ["(" ident {"," ident} ")"].
    fn formal_args(&mut self) -> Result<Vec<Node<String>>> {
        let mut args = Vec::new();
        if self.cur.tag == Tag::LParen {
            self.read();
            let arg = self.get_ident()?;
            args.push(arg);
            while self.cur.tag == Tag::Comma {
                self.read();
                let arg = self.get_ident()?;
                args.push(arg);
            }
            self.expect(Tag::RParen)?;
        }
        Ok(args)
    }

    // ProcDecl = "proc" ident FormalArgs ";" ProcBody ident.
    fn proc_decl(&mut self) -> Result<Proc> {
        self.expect(Tag::Proc)?;
        let proc_name = self.get_ident()?;
        let args = self.formal_args()?;
        self.expect(Tag::Semicolon)?;
        let mut new_proc_list = Vec::new();
        let stat_list = self.proc_body(&mut new_proc_list)?;
        let end_proc_name = self.get_ident()?;
        if proc_name != end_proc_name {
            return Err(Error::ProcNameMatch(self.cur.clone()));
        }
        let proc = Proc {
            proc_name,
            args,
            proc_list: new_proc_list,
            stat_list,
            symbols: Vec::new(),
        };
        Ok(proc)
    }

    // Decls = {ProcDecl ";"}
    fn decls(&mut self) -> Result<Vec<Proc>> {
        let mut proc_list = Vec::new();
        if self.cur.tag == Tag::Proc {
            let proc = self.proc_decl()?;
            proc_list.push(proc);
            while self.cur.tag == Tag::Semicolon {
                self.read();
                let proc = self.proc_decl()?;
                proc_list.push(proc);
            }
        }
        Ok(proc_list)
    }

    // SymbolDecl = "symbol" {symbol ";"}.
    fn symbol_decl(&mut self) -> Result<Vec<char>> {
        let mut symbols = Vec::new();
        self.expect(Tag::Symbol)?;
        if let Tag::Sym(s) = self.cur.tag {
            symbols.push(s);
            self.read();
            while self.cur.tag == Tag::Semicolon {
                self.read();
                let s = self.get_sym()?;
                symbols.push(s);
            }
        }
        Ok(symbols)
    }


    // Prog = "program" ident ";" SymbolDecl Decls "begin" StatList "end" ident "." EOF.
    pub fn parse(&mut self) -> Result<Proc> {
        self.read();
        self.expect(Tag::Program)?;
        let proc_name = self.get_ident()?;
        self.expect(Tag::Semicolon)?;
        let symbols = self.symbol_decl()?;
        let proc_list = self.decls()?;
        self.expect(Tag::Begin)?;
        let stat_list = self.stat_list()?;
        self.expect(Tag::End)?;
        let end_proc_name = self.get_ident()?;
        if proc_name != end_proc_name {
            return Err(Error::ProcNameMatch(self.cur.clone()));
        }
        self.expect(Tag::Period)?;
        self.expect(Tag::Eof)?;
        let program = Proc {
            proc_name,
            args: Vec::new(),
            symbols,
            proc_list,
            stat_list,
        };
        Ok(program)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_token(v: Vec<Tag>) -> Vec<Token> {
        let mut u = Vec::new();
        let mut j = 0;
        for tag in v {
            let token = Token {
                line: 0,
                pos: j,
                tag,
            };
            u.push(token);
            j += 1;
        }
        u
    }

    fn make_node<T: PartialEq>(node: T) -> Node<T> {
        Node {
            line: 0,
            pos: 0,
            node,
        }
    }

    #[test]
    fn minimal() {
        let test = "test".to_string();
        let test_file = vec![
            Tag::Program,
            Tag::Ident(test.clone()),
            Tag::Semicolon,
            Tag::Symbol,
            Tag::Sym('0'),
            Tag::Begin,
            Tag::Left,
            Tag::End,
            Tag::Ident(test.clone()),
            Tag::Period,
        ];
        let stream = make_token(test_file);
        let mut p = Parser::new(stream);
        let ans = Proc {
            proc_name: make_node(test),
            args: Vec::new(),
            symbols: vec!['0'],
            proc_list: Vec::new(),
            stat_list: vec![Stat::Left],
        };
        assert_eq!(p.parse().unwrap(), ans)
    }

    #[test]
    fn decl_proc() {
        let test = "test".to_string();
        let test_proc = "test_proc".to_string();
        let x = "x".to_string();
        let test_file = vec![
            Tag::Program,
            Tag::Ident(test.clone()),
            Tag::Semicolon,
            Tag::Symbol,
            Tag::Sym('0'),
            Tag::Proc,
            Tag::Ident(test_proc.clone()),
            Tag::LParen,
            Tag::Ident(x.clone()),
            Tag::RParen,
            Tag::Semicolon,
            Tag::Begin,
            Tag::Print,
            Tag::LParen,
            Tag::Ident(x.clone()),
            Tag::RParen,
            Tag::End,
            Tag::Ident(test_proc.clone()),
            Tag::Begin,
            Tag::Ident(test_proc.clone()),
            Tag::LParen,
            Tag::Sym('0'),
            Tag::RParen,
            Tag::End,
            Tag::Ident(test.clone()),
            Tag::Period,
        ];
        let mut p = Parser::new(make_token(test_file));
        let ans_test_proc = Proc {
            proc_name: make_node(test_proc.clone()),
            args: vec![make_node(x.clone())],
            symbols: Vec::new(),
            proc_list: Vec::new(),
            stat_list: vec![Stat::Print(Val::Var(make_node(x)))],
        };
        let ans_test = Proc {
            proc_name: make_node(test),
            args: Vec::new(),
            symbols: vec!['0'],
            proc_list: vec![ans_test_proc],
            stat_list: vec![Stat::Call(make_node(test_proc), vec![Val::Sym(make_node('0'))])],
        };
        assert_eq!(p.parse().unwrap(), ans_test)
    }

    #[test]
    fn if_stat() {
        let test = "test".to_string();
        let test_file = vec![
            Tag::Program,
            Tag::Ident(test.clone()),
            Tag::Semicolon,
            Tag::Symbol,
            Tag::Sym('0'),
            Tag::Begin,
            Tag::If,
            Tag::Sym('0'),
            Tag::Then,
            Tag::Left,
            Tag::Elsif,
            Tag::Sym('0'),
            Tag::Then,
            Tag::Right,
            Tag::Else,
            Tag::Left,
            Tag::End,
            Tag::End,
            Tag::Ident(test.clone()),
            Tag::Period,
        ];
        let stream = make_token(test_file);
        let mut p = Parser::new(stream);
        let ans_if = Stat::If(Expr::Val(Val::Sym(make_node('0'))), vec![Stat::Left],
        vec![Stat::If(Expr::Val(Val::Sym(make_node('0'))), vec![Stat::Right],
        vec![Stat::Left])]);
        let ans = Proc {
            proc_name: make_node(test),
            args: Vec::new(),
            symbols: vec!['0'],
            proc_list: Vec::new(),
            stat_list: vec![ans_if],
        };
        assert_eq!(p.parse().unwrap(), ans)
    }
    
    #[test]
    fn while_stat() {
        let test = "test".to_string();
        let test_file = vec![
            Tag::Program,
            Tag::Ident(test.clone()),
            Tag::Semicolon,
            Tag::Symbol,
            Tag::Sym('0'),
            Tag::Begin,
            Tag::While,
            Tag::Sym('0'),
            Tag::Do,
            Tag::Left,
            Tag::End,
            Tag::End,
            Tag::Ident(test.clone()),
            Tag::Period,
        ];
        let stream = make_token(test_file);
        let mut p = Parser::new(stream);
        let ans_while = Stat::While(Expr::Val(Val::Sym(make_node('0'))), vec![Stat::Left]);
        let ans = Proc {
            proc_name: make_node(test),
            args: Vec::new(),
            symbols: vec!['0'],
            proc_list: Vec::new(),
            stat_list: vec![ans_while],
        };
        assert_eq!(p.parse().unwrap(), ans)
    }
    
    #[test]
    fn repeat_stat() {
        let test = "test".to_string();
        let test_file = vec![
            Tag::Program,
            Tag::Ident(test.clone()),
            Tag::Semicolon,
            Tag::Symbol,
            Tag::Sym('0'),
            Tag::Begin,
            Tag::Repeat,
            Tag::Left,
            Tag::Until,
            Tag::Sym('0'),
            Tag::End,
            Tag::Ident(test.clone()),
            Tag::Period,
        ];
        let stream = make_token(test_file);
        let mut p = Parser::new(stream);
        let ans_repeat = Stat::Repeat(vec![Stat::Left], Expr::Val(Val::Sym(make_node('0'))));
        let ans = Proc {
            proc_name: make_node(test),
            args: Vec::new(),
            symbols: vec!['0'],
            proc_list: Vec::new(),
            stat_list: vec![ans_repeat],
        };
        assert_eq!(p.parse().unwrap(), ans)
    }

    #[test]
    fn expr() {
        let test = "test".to_string();
        let test_file = vec![
            Tag::Program,
            Tag::Ident(test.clone()),
            Tag::Semicolon,
            Tag::Symbol,
            Tag::Sym('0'),
            Tag::Begin,
            Tag::If,
            Tag::Not,
            Tag::LParen,
            Tag::Sym('0'),
            Tag::And,
            Tag::Sym('0'),
            Tag::Or,
            Tag::Sym('0'),
            Tag::RParen,
            Tag::Then,
            Tag::Left,
            Tag::End,
            Tag::End,
            Tag::Ident(test.clone()),
            Tag::Period,
        ];
        let stream = make_token(test_file);
        let mut p = Parser::new(stream);
        let val_zero = Box::new(Expr::Val(Val::Sym(make_node('0'))));
        let ans_and = Box::new(Expr::And(val_zero.clone(), val_zero.clone()));
        let ans_or = Box::new(Expr::Or(ans_and, val_zero.clone()));
        let ans_expr = Expr::Not(ans_or);
        let ans_if = Stat::If(ans_expr, vec![Stat::Left], Vec::new());
        let ans = Proc {
            proc_name: make_node(test),
            args: Vec::new(),
            symbols: vec!['0'],
            proc_list: Vec::new(),
            stat_list: vec![ans_if],
        };
        assert_eq!(p.parse().unwrap(), ans)
    }
}

