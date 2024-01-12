// This module contains functionality for converting Extended While programs into Core While programs.

use crate::parser::{Block, Expression, Prog, Statement};

// Converting numbers, lists, and switch statements to core while is straight forward

pub fn num_to_core(n: usize) -> Expression {
    match n {
        0 => Expression::Nil,
        _ => Expression::Cons(Expression::Nil.into(), num_to_core(n - 1).into()),
    }
}

pub fn list_to_core(list: &[Expression]) -> Expression {
    match list {
        [] => Expression::Nil,
        v => Expression::Cons(v[0].clone().into(), list_to_core(&v[1..]).into()),
    }
}

pub fn switch_to_ifs(
    cond: &Expression,
    cases: &Vec<(Expression, Block)>,
    default: &Block,
) -> Statement {
    use Statement::*;

    fn recur(
        cond: &Expression,
        v: &[(Expression, Vec<Statement>)],
        default: Vec<Statement>,
    ) -> Vec<Statement> {
        match v {
            [] => default,
            v => {
                let (case, stmts) = &v[0];
                vec![If {
                    cond: Expression::Eq(Box::new(case.clone()), Box::new(cond.clone())),
                    then: Block(stmts.clone()),
                    or: Block(recur(cond, &v[1..], default)),
                }]
            }
        }
    }

    let v = cases
        .iter()
        .map(|(case, block)| (case.clone(), block.0.clone()))
        .collect::<Vec<_>>();
    recur(cond, &v[..], default.0.clone())[0].clone()
}

// macros are slightly more annoying as we have to replace all the conflicting variable names

pub fn macros_to_core(_prog: &Prog) -> anyhow::Result<Prog> {
    todo!()
}

// equals is the worst case, as the equals expression has to be replaced with a new expression... and entirely new statement has to be added! Nested equals get particularly head-scratching to work out.

pub fn equals_to_core(_prog: &Prog) -> Prog {
    todo!()
}
