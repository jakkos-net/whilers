use std::fmt::Display;

#[derive(Debug, Eq, Clone)]
pub enum NilTree {
    Nil,
    Node {
        left: Box<NilTree>,
        right: Box<NilTree>,
    },
}

impl NilTree {
    // hd and tl have to be implemented this way due to implementing the drop trait
    pub fn hd(mut self) -> NilTree {
        match &mut self {
            NilTree::Nil => NilTree::Nil,
            NilTree::Node {
                ref mut left,
                right: _,
            } => std::mem::replace(left, NilTree::Nil),
        }
    }
    pub fn tl(mut self) -> NilTree {
        match &mut self {
            NilTree::Nil => NilTree::Nil,
            NilTree::Node {
                left: _,
                ref mut right,
            } => std::mem::replace(right, NilTree::Nil),
        }
    }
}

// impl Clone for NilTree {
//     fn clone(&self) -> Self {
//         todo!()
//     }
// }

impl PartialEq for NilTree {
    fn eq(&self, other: &Self) -> bool {
        let mut stack = vec![(self, other)];
        while !stack.is_empty() {
            let (a, b) = stack.pop().unwrap();
            match (a, b) {
                (&NilTree::Nil, &NilTree::Nil) => (),
                (&NilTree::Nil, _other) | (_other, &NilTree::Nil) => return false,
                (
                    &NilTree::Node {
                        left: ref a_left,
                        right: ref a_right,
                    },
                    &NilTree::Node {
                        left: ref b_left,
                        right: ref b_right,
                    },
                ) => {
                    stack.push((&a_left, &b_left));
                    stack.push((&a_right, &b_right))
                }
            }
        }
        return true;
    }
}

impl Display for NilTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        enum Item<'a> {
            Node(&'a NilTree),
            Char(char),
        }

        let mut stack = vec![Item::Node(self)];
        while let Some(item) = stack.pop() {
            match item {
                Item::Node(node) => match node {
                    NilTree::Nil => {
                        "nil".fmt(f)?;
                    }
                    NilTree::Node { left, right } => {
                        stack.push(Item::Char('>'));
                        stack.push(Item::Node(right));
                        stack.push(Item::Char('.'));
                        stack.push(Item::Node(left));
                        stack.push(Item::Char('<'));
                    }
                },
                Item::Char(c) => c.fmt(f)?,
            }
        }
        Ok(())
    }
}

impl Drop for NilTree {
    fn drop(&mut self) {
        match self {
            Self::Nil => {}
            Self::Node { left, right } => {
                let mut stack = vec![
                    std::mem::replace(&mut **left, Self::Nil),
                    std::mem::replace(&mut **right, Self::Nil),
                ];
                while let Some(mut node) = stack.pop() {
                    match &mut node {
                        Self::Nil => {}
                        Self::Node { left, right } => {
                            stack.push(std::mem::replace(&mut **left, Self::Nil));
                            stack.push(std::mem::replace(&mut **right, Self::Nil));
                        }
                    }
                }
            }
        }
    }
}
