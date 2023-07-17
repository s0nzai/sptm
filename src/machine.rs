
use std::fmt;
use crate::quintuples::{Move, Quintuples, Action};

#[derive(Clone, Debug, PartialEq)]
pub struct Config {
    pub state: u64,
    left_tape: Vec<char>,
    right_tape: Vec<char>,
}

impl Config {
    pub fn new(initial_string: String) -> Self {
        let mut tape: Vec<char> = initial_string.chars().collect();
        tape.reverse();
        tape.push(' ');
        Self {
            left_tape: Vec::new(),
            state: 0,
            right_tape: tape,
        }
    }
    pub fn move_left(&mut self) {
        match self.left_tape.pop() {
            None => self.right_tape.push(' '),
            Some(c) => self.right_tape.push(c),
        }
    }
    pub fn move_right(&mut self) {
        let c = self.right_tape.pop().unwrap();
        self.left_tape.push(c);
        if self.right_tape.is_empty() {
            self.right_tape.push(' ');
        }
    }
    pub fn print(&mut self, c: char) {
        self.right_tape.pop();
        self.right_tape.push(c);
    }
    pub fn read(&self) -> char {
        *self.right_tape.last().unwrap()
    }
}

impl fmt::Display for Config {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:3}: ", self.state)?;
        for c in &self.left_tape {
            write!(f, "{}", c)?;
        };
        let mut right_tape = self.right_tape.clone();
        right_tape.reverse();
        write!(f, "[{}]", right_tape[0])?;
        for c in &right_tape[1..] {
            write!(f, "{}", c)?;
        };
        write!(f, "")
    }
}

pub struct Machine {
    quadruples: Quintuples,
    pub config: Config,
}

impl Machine {

    pub fn new(quadruples: Quintuples, initial_config: Config) -> Self {
        Self {
            quadruples,
            config: initial_config,
        }
    }

    pub fn run_step(&mut self) -> Option<&Config> {
        let q = self.config.state;
        let read = self.config.read();
        if let Some(Action(write, mv, next_state)) = self.quadruples.get(q, read) {
            self.config.print(*write);
            match mv {
                Move::NoMove => {},
                Move::Left => self.config.move_left(),
                Move::Right => self.config.move_right(),
            }
            self.config.state = *next_state;
            Some(&self.config)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_machine() {
        let mut m = Machine::new(Quintuples::new(Vec::new()), Config::new(String::new()));
        assert!(m.run_step().is_none())
    }

    #[test]
    fn print() {
        let mut q = Quintuples::new(vec![' ', '1']);
        q.insert(0, ' ', Action('1', Move::NoMove, 1));
        let mut m = Machine::new(q, Config::new("1".to_string()));
        let ans = Config {
            state: 1,
            left_tape: Vec::new(),
            right_tape: vec!['1', '1'],
        };
        assert_eq!(*m.run_step().unwrap(), ans)
    }
    
    #[test]
    fn left() {
        let mut q = Quintuples::new(vec![' ', '1']);
        q.insert(0, ' ', Action('1', Move::Left, 1));
        let mut m = Machine::new(q, Config::new("1".to_string()));
        let ans = Config {
            state: 1,
            left_tape: Vec::new(),
            right_tape: vec!['1', '1', ' '],
        };
        assert_eq!(*m.run_step().unwrap(), ans)
    }
    
    #[test]
    fn right() {
        let mut q = Quintuples::new(vec![' ', '1']);
        q.insert(0, ' ', Action('1', Move::Right, 1));
        let mut m = Machine::new(q, Config::new(String::from("1")));
        let ans = Config {
            state: 1,
            left_tape: vec!['1'],
            right_tape: vec!['1'],
        };
        assert_eq!(*m.run_step().unwrap(), ans)
    }
}

