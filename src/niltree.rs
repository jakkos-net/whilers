use std::{fmt::Display, sync::Arc};

use crate::extended_to_core::num_to_niltree;

#[derive(Debug, Eq, Clone)]
pub enum NilTree {
    Nil,
    List(Arc<Vec<NilTree>>),
    Num(usize),
}

impl NilTree {
    pub fn list(v: Vec<NilTree>) -> NilTree {
        NilTree::List(Arc::new(v))
    }

    pub fn as_bool(&self) -> bool {
        match self {
            NilTree::Nil => false,
            NilTree::List(a) => !a.is_empty(),
            NilTree::Num(n) => *n != 0,
        }
    }
}

impl PartialEq for NilTree {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (NilTree::Nil, NilTree::Nil) => true,
            (NilTree::Nil, NilTree::List(list)) | (NilTree::List(list), NilTree::Nil) => {
                list.is_empty()
            }
            (NilTree::Nil, NilTree::Num(n)) | (NilTree::Num(n), NilTree::Nil) => *n == 0,

            (NilTree::Num(a), NilTree::Num(b)) => a == b,

            (NilTree::List(a), NilTree::List(b)) => a == b,
            (list, NilTree::Num(n)) | (NilTree::Num(n), list) => &num_to_niltree(*n) == list,
        }
    }
}

impl NilTree {
    pub fn hd(&self) -> NilTree {
        match self {
            NilTree::Nil | NilTree::Num(_) => NilTree::Nil,
            NilTree::List(v) => v.last().cloned().unwrap_or(NilTree::Nil),
        }
    }
    pub fn tl(&self) -> NilTree {
        match self {
            NilTree::Nil => NilTree::Nil,
            NilTree::List(v) => {
                if v.is_empty() {
                    NilTree::Nil
                } else {
                    let v = v[0..v.len() - 1].into();
                    NilTree::list(v)
                }
            }
            NilTree::Num(n) => {
                let n = if *n == 0 { 0 } else { n - 1 };
                NilTree::Num(n)
            }
        }
    }
}

pub fn cons(a: &NilTree, b: &NilTree) -> NilTree {
    match b {
        NilTree::Nil => NilTree::list(vec![a.clone()]),
        NilTree::List(v) => {
            let mut v = (**v).clone();
            v.push(a.clone());
            NilTree::list(v)
        }
        NilTree::Num(b) => {
            let b = &num_to_niltree(*b);
            cons(a, b)
        }
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
            NilTree::Num(n) => num_to_niltree(*n as usize).fmt(f),
        }
    }
}
