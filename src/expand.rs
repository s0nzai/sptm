
use crate::tree::*;
use crate::error::*;
use std::collections::{HashMap, HashSet};

#[derive(Clone)]
enum EnvVal {
    Proc(Proc),
    Sym(Node<char>),
    Var(Node<String>),
}

impl From<Val> for EnvVal {
    fn from(item: Val) -> Self {
        match item {
            Val::Sym(s) => Self::Sym(s),
            Val::Var(v) => Self::Var(v),
        }
    }
}

pub struct Expand {
    symbols: HashSet<char>,
    env: Vec<HashMap<String, EnvVal>>,
}

impl Expand {

    pub fn new() -> Self {
        let mut symbols = HashSet::new();
        symbols.insert(' ');
        Self {
            symbols,
            env: Vec::new(),
        }
    }

    fn expand_symbols(&mut self, symbols: &[Node<char>]) -> Result<()> {
        for s in symbols {
            if !self.symbols.insert(s.node) {
                let e = Error::new(s.line, s.pos, ErrorKind::DupSymbol(s.node));
                return Err(e);
            }
        }
        Ok(())
    }

    fn get_val(&self, id: &Node<String>) -> Result<Val> {
        for table in self.env.iter().rev() {
            match table.get(&id.node) {
                Some(EnvVal::Sym(s)) => {
                    return Ok(Val::Sym(s.clone()))
                },
                Some(EnvVal::Proc(_)) => {
                    let e = Error::new(id.line, id.pos, ErrorKind::IsProc(id.node.clone()));
                    return Err(e);
                },
                Some(EnvVal::Var(v)) => {
                    return Ok(Val::Var(v.clone()))
                }
                None => (),
            }
        }
        let e = Error::new(id.line, id.pos, ErrorKind::NotFound(id.node.clone()));
        Err(e)
    }
    
    fn get_proc(&self, id: &Node<String>) -> Result<Proc> {
        for table in self.env.iter().rev() {
            match table.get(&id.node) {
                Some(EnvVal::Proc(proc)) => {
                    return Ok(proc.clone())
                },
                Some(_) => {
                    let e = Error::new(id.line, id.pos, ErrorKind::IsProc(id.node.clone()));
                    return Err(e);
                },
                None => (),
            }
        }
        let e = Error::new(id.line, id.pos, ErrorKind::NotFound(id.node.clone()));
        Err(e)
    }

    fn add_env(&mut self, id: &Node<String>, val: EnvVal) -> Result<()> {
        let table = self.env.last_mut().unwrap();
        if table.insert(id.node.clone(), val).is_some() {
            let e = Error::new(id.line, id.pos, ErrorKind::DupIdent(id.node.clone()));
            return Err(e);
        }
        Ok(())
    }

    fn push_env(&mut self) {
        let table = HashMap::new();
        self.env.push(table);
    }

    fn pop_env(&mut self) {
        self.env.pop();
    }

    fn expand_val(&self, val: Val) -> Result<Val> {
        match val {
            Val::Sym(s) => {
                if !self.symbols.contains(&s.node) {
                    let e = Error::new(s.line, s.pos, ErrorKind::NotSym(s.node));
                    return Err(e);
                }
                Ok(Val::Sym(s))
            },
            Val::Var(v) => {
                match self.get_val(&v)? {
                    Val::Sym(s) => {
                        if !self.symbols.contains(&s.node) {
                            let e = Error::new(s.line, s.pos, ErrorKind::NotSym(s.node));
                            return Err(e);
                        }
                        Ok(Val::Sym(s))
                    },
                    Val::Var(v) => Ok(Val::Var(v))
                }
            }
        }
    }

    fn expand_call(&mut self, id: Node<String>, args: Vec<Val>) -> Result<Vec<Stat>> {
        let proc = self.get_proc(&id)?;
        if proc.args.len() != args.len() {
            let e = Error::new(id.line, id.pos, ErrorKind::ArgsNum(id.node.clone()));
            return Err(e);
        }
        let mut actual = Vec::new();
        for arg in args {
            let val = self.expand_val(arg)?;
            actual.push(val);
        }
        let proc = self.expand_proc(proc, actual)?;
        Ok(proc.stat_list)
    }

    fn expand_expr(&self, e: Expr) -> Result<Expr> {
        match e {
            Expr::Val(val) => {
                let val = self.expand_val(val)?;
                Ok(Expr::Val(val))
            },
            Expr::Or(lhs, rhs) => {
                let lhs = self.expand_expr(*lhs)?;
                let rhs = self.expand_expr(*rhs)?;
                Ok(Expr::Or(Box::new(lhs), Box::new(rhs))) 
            },
            Expr::And(lhs, rhs) => {
                let lhs = self.expand_expr(*lhs)?;
                let rhs = self.expand_expr(*rhs)?;
                Ok(Expr::And(Box::new(lhs), Box::new(rhs))) 
            },
            Expr::Not(e) => {
                let e = self.expand_expr(*e)?;
                Ok(Expr::Not(Box::new(e)))
            },
        }
    }

    fn expand_if(&mut self, e: Expr, stat_list: Vec<Stat>, else_stat: Vec<Stat>) -> Result<Stat> {
        let e = self.expand_expr(e)?;
        let stat_list = self.expand_stat_list(stat_list)?;
        let else_stat = self.expand_stat_list(else_stat)?;
        let if_stat = Stat::If(e, stat_list, else_stat);
        Ok(if_stat)
    }
    
    fn expand_while(&mut self, e: Expr, stat_list: Vec<Stat>) -> Result<Stat> {
        let e = self.expand_expr(e)?;
        let stat_list = self.expand_stat_list(stat_list)?;
        let while_stat = Stat::While(e, stat_list);
        Ok(while_stat)
    }
    
    fn expand_repeat(&mut self, stat_list: Vec<Stat>, e: Expr) -> Result<Stat> {
        let stat_list = self.expand_stat_list(stat_list)?;
        let e = self.expand_expr(e)?;
        let repeat_stat = Stat::Repeat(stat_list, e);
        Ok(repeat_stat)
    }

    fn expand_stat(&mut self, stat: Stat) -> Result<Vec<Stat>> {
        match stat {
            Stat::Call(id, args) => self.expand_call(id, args),
            Stat::If(e, stat_list, else_stat) => {
                let if_stat = self.expand_if(e, stat_list, else_stat)?;
                Ok(vec![if_stat])
            },
            Stat::While(e, stat_list) => {
                let while_stat = self.expand_while(e, stat_list)?;
                Ok(vec![while_stat])
            },
            Stat::Repeat(stat_list, e) => {
                let repeat_stat = self.expand_repeat(stat_list, e)?;
                Ok(vec![repeat_stat])
            },
            Stat::Print(val) => {
                let val = self.expand_val(val)?;
                Ok(vec![Stat::Print(val)])
            },
            Stat::Left => Ok(vec![Stat::Left]),
            Stat::Right => Ok(vec![Stat::Right]),
            Stat::Erase => Ok(vec![Stat::Erase]),
        }
    }

    fn expand_stat_list(&mut self, stat_list: Vec<Stat>) -> Result<Vec<Stat>> {
        let mut new_stat_list = Vec::new();
        for stat in stat_list {
            let mut expanded = self.expand_stat(stat)?;
            new_stat_list.append(&mut expanded);
        }
        Ok(new_stat_list)
    }

    pub fn expand_proc(&mut self, proc: Proc, args: Vec<Val>) -> Result<Proc> { 
        self.push_env();
        let mut j = 0;
        for arg in args {
            self.add_env(&proc.args[j], EnvVal::from(arg))?;
            j += 1;
        }
        self.expand_symbols(&proc.symbols)?;
        for proc_def in proc.proc_list {
            let mut actual = Vec::new();
            let args = proc_def.args.clone();
            for arg in args.clone() {
                actual.push(Val::Var(arg))
            }
            let mut proc_def = self.expand_proc(proc_def, actual)?;
            proc_def.args = args;
            self.add_env(&proc_def.proc_name, EnvVal::Proc(proc_def.clone()))?;
        }
        let stat_list = self.expand_stat_list(proc.stat_list)?;
        self.pop_env();
        let new_proc = Proc {
            symbols: proc.symbols,
            proc_list: Vec::new(),
            proc_name: proc.proc_name,
            args: Vec::new(),
            stat_list,
        };
        Ok(new_proc)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_node<T: PartialEq>(node: T) -> Node<T> {
        Node {
            line: 0,
            pos: 0,
            node,
        }
    }

    #[test]
    fn call() {
        let x = make_node("x".to_string());
        let f = make_node("f".to_string());
        let proc_f = Proc {
            proc_name: f.clone(),
            symbols: Vec::new(),
            proc_list: Vec::new(),
            args: vec![x.clone()],
            stat_list: vec![
                Stat::Print(Val::Var(x))
            ],
        };
        let test = make_node("test".to_string());
        let proc_test = Proc {
            proc_name: test.clone(),
            symbols: vec![make_node('0')],
            args: Vec::new(),
            proc_list: vec![proc_f],
            stat_list: vec![
                Stat::Call(f, vec![Val::Sym(make_node('0'))])
            ],
        };
        let ans = Proc {
            proc_name: test,
            symbols: vec![make_node('0')],
            args: Vec::new(),
            proc_list: Vec::new(),
            stat_list: vec![
                Stat::Print(Val::Sym(make_node('0'))),
            ],
        };
        let mut expand = Expand::new();
        assert_eq!(expand.expand_proc(proc_test, Vec::new()).unwrap(), ans)
    }
    
    #[test]
    fn boxing() {
        let x = make_node("x".to_string());
        let f = make_node("f".to_string());
        let g = make_node("g".to_string());
        let proc_f = Proc {
            proc_name: f,
            symbols: Vec::new(),
            proc_list: Vec::new(),
            args: vec![x.clone()],
            stat_list: vec![
                Stat::Print(Val::Var(x.clone()))
            ],
        };
        let proc_g = Proc {
            proc_name: g.clone(),
            args: vec![x.clone()],
            symbols: Vec::new(),
            proc_list: vec![proc_f],
            stat_list: vec![
                Stat::Print(Val::Var(x))
            ],
        };
        let test = make_node("test".to_string());
        let proc_test = Proc {
            proc_name: test.clone(),
            symbols: vec![make_node('0')],
            args: Vec::new(),
            proc_list: vec![proc_g],
            stat_list: vec![
                Stat::Call(g, vec![Val::Sym(make_node('0'))])
            ],
        };
        let ans = Proc {
            proc_name: test,
            symbols: vec![make_node('0')],
            args: Vec::new(),
            proc_list: Vec::new(),
            stat_list: vec![
                Stat::Print(Val::Sym(make_node('0'))),
            ],
        };
        let mut expand = Expand::new();
        assert_eq!(expand.expand_proc(proc_test, Vec::new()).unwrap(), ans)
    }
    
    #[test]
    fn nested() {
        let x = make_node("x".to_string());
        let y = make_node("y".to_string());
        let f = make_node("f".to_string());
        let g = make_node("g".to_string());
        let proc_f = Proc {
            proc_name: f.clone(),
            symbols: Vec::new(),
            proc_list: Vec::new(),
            args: vec![y.clone()],
            stat_list: vec![
                Stat::Print(Val::Var(x.clone())),
                Stat::Print(Val::Var(y)),
            ],
        };
        let proc_g = Proc {
            proc_name: g.clone(),
            args: vec![x.clone()],
            symbols: Vec::new(),
            proc_list: vec![proc_f],
            stat_list: vec![
                Stat::Call(f, vec![Val::Sym(make_node('0'))])
            ],
        };
        let test = make_node("test".to_string());
        let proc_test = Proc {
            proc_name: test.clone(),
            symbols: vec![make_node('0'), make_node('1')],
            args: Vec::new(),
            proc_list: vec![proc_g],
            stat_list: vec![
                Stat::Call(g, vec![Val::Sym(make_node('1'))])
            ],
        };
        let ans = Proc {
            proc_name: test,
            symbols: vec![make_node('0'), make_node('1')],
            args: Vec::new(),
            proc_list: Vec::new(),
            stat_list: vec![
                Stat::Print(Val::Sym(make_node('1'))),
                Stat::Print(Val::Sym(make_node('0'))),
            ],
        };
        let mut expand = Expand::new();
        assert_eq!(expand.expand_proc(proc_test, Vec::new()).unwrap(), ans)
    }
}

