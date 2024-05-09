use std::fmt::Display;

use crate::extended_to_core::num_to_nils;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum NilTree {
    Nil,
    List(Vec<NilTree>),
    Num(usize),
}

impl NilTree {
    pub fn hd(self) -> NilTree {
        self.split().0
    }
    pub fn tl(self) -> NilTree {
        self.split().1
    }
    pub fn split(self) -> (NilTree, NilTree) {
        match self {
            NilTree::Nil => (NilTree::Nil, NilTree::Nil),
            NilTree::List(mut v) => {
                let h = v.pop().unwrap_or(NilTree::Nil);
                let t = if v.is_empty() {
                    NilTree::Nil
                } else {
                    NilTree::List(v)
                };

                (h, t)
            }
            NilTree::Num(n) => match n {
                0 | 1 => (NilTree::Nil, NilTree::Nil),
                n => (NilTree::Nil, NilTree::Num(n - 1)),
            },
        }
    }
    pub fn equivalent(&self, other: &NilTree) -> bool {
        use NilTree as N;
        match (self, other) {
            (N::Num(n), N::Nil) | (N::Nil, N::Num(n)) => *n == 0,
            (N::Num(a), N::Num(b)) => a == b,
            (N::Num(n), x) | (x, N::Num(n)) => &num_to_nils(*n) == x,
            (a, b) => a == b,
        }
    }
}

pub fn cons(a: NilTree, b: NilTree) -> NilTree {
    match b {
        NilTree::Nil => NilTree::List(vec![a]),
        NilTree::List(mut v) => {
            v.push(a);
            NilTree::List(v)
        }
        NilTree::Num(n) => match a {
            NilTree::Nil => NilTree::Num(n + 1),
            a => cons(a, num_to_nils(n)),
        },
    }
}

impl Display for NilTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NilTree::Nil => "nil".fmt(f),
            NilTree::List(v) => {
                let mut s = "".to_string();
                v.iter().rev().for_each(|nt| {
                    s.push_str("<");
                    s.push_str(nt.to_string().as_str());
                    s.push_str(".")
                });

                s.push_str("nil");

                for _ in 0..v.len() {
                    s.push_str(">")
                }

                s.fmt(f)
            }
            NilTree::Num(n) => num_to_nils(*n).fmt(f),
        }
    }
}
