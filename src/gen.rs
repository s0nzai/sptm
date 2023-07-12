
use crate::tree::*;
use crate::quintuples::{Action, Move, Quintuples};
use std::collections::HashSet;

pub struct Gen {
    symbols: HashSet<char>,
    cur_print: Option<char>,
}

impl Gen {

    pub fn new() -> Self {
        let mut symbols = HashSet::new();
        symbols.insert(' ');
        Self {
            symbols,
            cur_print: None,
        }
    }

    fn gen_expr(&self, e: Expr) -> HashSet<char> {
        match e {
            Expr::Val(Val::Sym(s)) => {
                let mut singleton = HashSet::new();
                singleton.insert(s.node);
                singleton
            },
            Expr::Not(e) => {
                let set_e = self.gen_expr(*e);
                let not = self.symbols.difference(&set_e).copied().collect();
                not
            },
            Expr::And(lhs, rhs) => {
                let set_lhs = self.gen_expr(*lhs);
                let set_rhs = self.gen_expr(*rhs);
                let and = set_lhs.intersection(&set_rhs).copied().collect();
                and
            },
            Expr::Or(lhs, rhs) => {
                let set_lhs = self.gen_expr(*lhs);
                let set_rhs = self.gen_expr(*rhs);
                let or = set_lhs.union(&set_rhs).copied().collect();
                or
            },
            _ => {
                panic!();
            }
        }
    }

    fn gen_jump(&self, q: &mut Quintuples, state: u64, dest: u64) {
        for s in &self.symbols {
            let action = Action(*s, Move::NoMove, dest);
            q.insert(state, *s, action)
        }
    }

    fn gen_if(&mut self, q: &mut Quintuples, e: Expr, stat_list: Vec<Stat>, else_stat: Vec<Stat>) {
        let cond = self.gen_expr(e);
        let state = q.final_state;
        for s in &cond {
            let action = Action(*s, Move::NoMove, state + 1);
            q.insert(state, *s, action);
        }
        self.gen_stat_list(q, stat_list);
        let else_state = q.final_state;
        self.gen_jump(q, else_state, 0);
        let else_cond = self.symbols.difference(&cond);
        for s in else_cond {
            let action = Action(*s, Move::NoMove, else_state + 1);
            q.insert(state, *s, action);
        }
        self.gen_stat_list(q, else_stat);
        self.gen_jump(q, else_state, q.final_state);
    }

    fn gen_while(&mut self, q: &mut Quintuples, e: Expr, stat_list: Vec<Stat>) {
        let cond = self.gen_expr(e);
        let state = q.final_state;
        self.gen_jump(q, state, state + 1);
        self.gen_stat_list(q, stat_list);
        self.gen_jump(q, q.final_state, state);
        let break_cond = self.symbols.difference(&cond);
        let final_state = q.final_state;
        for s in break_cond {
            let action = Action(*s, Move::NoMove, final_state);
            q.insert(state, *s, action);
        }
    }
    
    fn gen_repeat(&mut self, q: &mut Quintuples, stat_list: Vec<Stat>, e: Expr) {
        let state = q.final_state;
        self.gen_stat_list(q, stat_list);
        let cond = self.gen_expr(e);
        let final_state = q.final_state;
        for s in &self.symbols {
            if cond.contains(s) {
                let action = Action(*s, Move::NoMove, final_state + 1);
                q.insert(final_state, *s, action);
            } else {
                let action = Action(*s, Move::NoMove, state);
                q.insert(final_state, *s, action);
            }
        }
    }

    fn gen_print(&mut self, q: &mut Quintuples) {
        if let Some(s) = self.cur_print {
            let final_state = q.final_state;
            let action = Action(s, Move::NoMove, final_state + 1);
            for s in &self.symbols {
                q.insert(final_state, *s, action.clone());
            }
            self.cur_print = None;
        }
    }

    fn gen_move(&mut self, q: &mut Quintuples, mv: Move) {
        let final_state = q.final_state;
        if let Some(s) = self.cur_print {
            let action = Action(s, mv, final_state + 1);
            for s in &self.symbols {
                q.insert(final_state, *s, action.clone());
            }
            self.cur_print = None;
        } else {
            for s in &self.symbols {
                let action = Action(*s, mv, final_state + 1);
                q.insert(final_state, *s, action);
            }
        }
    }

    fn gen_stat_list(&mut self, q: &mut Quintuples, stat_list: Vec<Stat>) {
        for stat in stat_list {
            match stat {
                Stat::If(e, stat_list, else_stat) => {
                    self.gen_print(q);
                    self.gen_if(q, e, stat_list, else_stat)
                },
                Stat::While(e, stat_list) => {
                    self.gen_print(q);
                    self.gen_while(q, e, stat_list)
                },
                Stat::Repeat(stat_list, e) => {
                    self.gen_print(q);
                    self.gen_repeat(q, stat_list, e)
                },
                Stat::Print(Val::Sym(s)) => {
                    self.cur_print = Some(s.node);
                },
                Stat::Erase => {
                    self.cur_print = Some(' ');
                },
                Stat::Right => self.gen_move(q, Move::Right),
                Stat::Left => self.gen_move(q, Move::Left),
                _ => panic!(),
            }
        }
        self.gen_print(q);
    }

    pub fn gen(&mut self, proc: Proc) -> Quintuples {
        let mut symbols = vec![' '];
        for s in proc.symbols {
            symbols.push(s.node);
            self.symbols.insert(s.node);
        }
        let mut q = Quintuples::new(symbols);
        self.gen_stat_list(&mut q, proc.stat_list);
        q
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
    fn if_stat() {
        let mut gen = Gen::new();
        let test = make_node("test".to_string());
        let test_proc = Proc {
            proc_name: test.clone(),
            args: Vec::new(),
            symbols: vec![make_node('0')],
            proc_list: Vec::new(),
            stat_list: vec![
                Stat::If(Expr::Val(Val::Sym(make_node('0'))),
                vec![Stat::Right],
                vec![Stat::Left])
            ],
        };
        let q = gen.gen(test_proc);
        let mut ans = Quintuples::new(vec![' ', '0']);
        ans.insert(0, ' ', Action(' ', Move::NoMove, 3));
        ans.insert(0, '0', Action('0', Move::NoMove, 1));
        ans.insert(1, ' ', Action(' ', Move::Right, 2));
        ans.insert(1, '0', Action('0', Move::Right, 2));
        ans.insert(2, ' ', Action(' ', Move::NoMove, 4));
        ans.insert(2, '0', Action('0', Move::NoMove, 4));
        ans.insert(3, ' ', Action(' ', Move::Left, 4));
        ans.insert(3, '0', Action('0', Move::Left, 4));
        assert_eq!(q, ans)
    }
    
    #[test]
    fn while_stat() {
        let mut gen = Gen::new();
        let test = make_node("test".to_string());
        let test_proc = Proc {
            proc_name: test.clone(),
            args: Vec::new(),
            symbols: vec![make_node('0')],
            proc_list: Vec::new(),
            stat_list: vec![
                Stat::While(Expr::Val(Val::Sym(make_node('0'))),
                vec![Stat::Right])
            ],
        };
        let q = gen.gen(test_proc);
        let mut ans = Quintuples::new(vec![' ', '0']);
        ans.insert(0, ' ', Action(' ', Move::NoMove, 3));
        ans.insert(0, '0', Action('0', Move::NoMove, 1));
        ans.insert(1, ' ', Action(' ', Move::Right, 2));
        ans.insert(1, '0', Action('0', Move::Right, 2));
        ans.insert(2, ' ', Action(' ', Move::NoMove, 0));
        ans.insert(2, '0', Action('0', Move::NoMove, 0));
        assert_eq!(q, ans)
    }

    #[test]
    fn repeat_stat() {
        let mut gen = Gen::new();
        let test = make_node("test".to_string());
        let test_proc = Proc {
            proc_name: test.clone(),
            args: Vec::new(),
            symbols: vec![make_node('0')],
            proc_list: Vec::new(),
            stat_list: vec![
                Stat::Repeat(vec![Stat::Right],
                             Expr::Val(Val::Sym(make_node('0'))))
            ],
        };
        let q = gen.gen(test_proc);
        let mut ans = Quintuples::new(vec![' ', '0']);
        ans.insert(0, ' ', Action(' ', Move::Right, 1));
        ans.insert(0, '0', Action('0', Move::Right, 1));
        ans.insert(1, ' ', Action(' ', Move::NoMove, 0));
        ans.insert(1, '0', Action('0', Move::NoMove, 2));
        assert_eq!(q, ans)
    }
}
