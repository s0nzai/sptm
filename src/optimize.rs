
use crate::quintuples::{Action, Move, Quintuples};
use std::collections::HashMap;

fn remove_no_move(q: Quintuples) -> Quintuples {
    let mut r = Quintuples::new(q.symbol.clone());
    for (state, read) in q.iter() {
        let mut j = state;
        let mut cur_print = Some(read);
        while let Some(s) = cur_print {
            if let Some(Action(print, mv, next_state)) = q.get(j, s) {
                match mv {
                    Move::NoMove => {
                        cur_print = Some(*print);
                        if *next_state == state {
                            let action = Action(*print, Move::NoMove, *next_state);
                            r.insert(state, read, action);
                            cur_print = None;
                        } else {
                            j = *next_state;
                        }
                    },
                    Move::Left | Move::Right => {
                        let action = Action(*print, *mv, *next_state);
                        r.insert(state, read, action);
                        cur_print = None;
                    }
                }
            } else {
                let action = Action(s, Move::NoMove, q.final_state);
                r.insert(state, read, action);
                cur_print = None;
            }
        }
    }
    r
}

fn duplicate(q: Quintuples) -> Quintuples {
    let mut r = Quintuples::new(q.symbol.clone());
    let mut map = HashMap::new();
    for j in 0..q.final_state {
        for k in j+1..q.final_state {
            let mut is_same = true;
            for s in &q.symbol {
                let action_1 = q.get(j, *s);
                let action_2 = q.get(k, *s);
                if action_1 != action_2 {
                    is_same = false;
                }
            }
            if is_same {
                map.insert(k, j);
            }
        }
    }
    for (state, read) in q.iter() {
        if map.contains_key(&state) {
            continue;
        }
        let Action(print, mv, next_state) = q.get(state, read).unwrap();
        let next_state = map.get(next_state).unwrap_or(next_state);
        let action = Action(*print, *mv, *next_state);
        r.insert(state, read, action);
    }
    r
}

fn renumbering(q: Quintuples) -> Quintuples {
    let mut r = Quintuples::new(q.symbol.clone());
    let mut j = 0;
    let mut map = HashMap::new();
    for (state, _) in q.iter() {
        if !map.contains_key(&state) {
            map.insert(state, j);
            j += 1;
        }
    }
    for (state, read) in q.iter() {
        let Action(print, mv, next_state) = q.get(state, read).unwrap();
        let next_state = if *next_state >= q.final_state {
            j
        } else {
            *map.get(next_state).unwrap_or(next_state)
        };
        let action = Action(*print, *mv, next_state);
        let state = map.get(&state).unwrap_or(&state);
        r.insert(*state, read, action);
    }
    r
}

pub fn optimize(q: Quintuples) -> Quintuples {
    let r = remove_no_move(q);
    let r = duplicate(r);
    let r = renumbering(r);
    r
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn remove_no_move() {
        let mut test_q = Quintuples::new(vec![' ', '0']);
        test_q.insert(0, ' ', Action(' ', Move::Right, 1));
        test_q.insert(0, '0', Action('0', Move::Right, 1));
        test_q.insert(1, ' ', Action(' ', Move::NoMove, 4));
        test_q.insert(1, '0', Action('0', Move::NoMove, 2));
        test_q.insert(2, ' ', Action(' ', Move::Right, 3));
        test_q.insert(2, '0', Action(' ', Move::Right, 3));
        test_q.insert(3, ' ', Action(' ', Move::NoMove, 1));
        test_q.insert(3, '0', Action('0', Move::NoMove, 1));
        let test_q = optimize(test_q);
        let mut ans = Quintuples::new(vec![' ', '0']);
        ans.insert(0, ' ', Action(' ', Move::Right, 1));
        ans.insert(1, ' ', Action(' ', Move::NoMove, 2));
        ans.insert(1, '0', Action(' ', Move::Right, 1));
        assert_eq!(test_q, ans)
    }
}

