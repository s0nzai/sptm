
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Move {
    NoMove,
    Left,
    Right,
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::NoMove => write!(f, "N"),
            Self::Left => write!(f, "L"),
            Self::Right => write!(f, "R"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Action(pub char, pub Move, pub u64);

impl fmt::Display for Action {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Action(print, mv, next_state) = self;
        write!(f, "{}{}/{:<3}", print, mv, next_state)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Quintuples {
    actions: HashMap<(u64, char), Action>,
    pub symbol: Vec<char>,
    pub final_state: u64,
}

impl Quintuples {
    pub fn new(symbol: Vec<char>) -> Self {
        Quintuples {
            actions: HashMap::new(),
            symbol,
            final_state: 0,
        }
    }

    pub fn get(&self, state: u64, read: char) -> Option<&Action> {
        self.actions.get(&(state, read))
    }

    pub fn insert(&mut self, state: u64, read: char, action: Action) {
        self.actions.insert((state, read), action);
        if state >= self.final_state {
            self.final_state = state + 1;
        }
    }
}

impl fmt::Display for Quintuples {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let symbols = self.symbol.clone();
        write!(f, "     ")?;
        for s in &symbols {
            if *s == ' ' {
                write!(f, "blank  ")?;
            } else {
                write!(f, "{}      ", *s)?;
            }
        }
        writeln!(f, "")?;
        for j in 0..self.final_state {
            write!(f, "{:3}| ", j)?;
            for s in &symbols {
                if let Some(action) = self.get(j, *s) {
                    write!(f, "{} ", action)?;
                } else {
                    write!(f, "------ ")?;
                }
            }
            writeln!(f, "")?;
        }
        write!(f, "")
    }
}

