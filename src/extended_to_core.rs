// This module contains functionality for converting Extended While programs into Core While programs.

use anyhow::Context;
use indexmap::{IndexMap, IndexSet};

use crate::{
    output::Varnums,
    parser::{Block, Expression, Prog, ProgName, Statement, VarName},
};

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
pub fn macros_to_core(
    main_prog: &Prog,
    main_prog_name: &ProgName,
    progs: &IndexMap<ProgName, Prog>,
) -> anyhow::Result<Prog> {
    let mut used_progs: IndexSet<ProgName> = Default::default();
    used_progs.insert(main_prog_name.clone());

    let mut main_prog = main_prog.clone();
    let used_vars = Varnums::new(&main_prog);

    // while there are still macros
    // get used varnames in prog
    // find a macro

    // replace macro with:
    //   assign(mapped_input_var, mapped_input_expr)
    //   rest of program block

    todo!()
}

fn replace_vars_in_block(
    block: &Block,
    mapping: &IndexMap<VarName, VarName>,
) -> anyhow::Result<Block> {
    let new_stmts = block
        .0
        .iter()
        .map(|stmt| {
            use Statement as S;
            Ok(match stmt {
                S::Assign(var, expr) => S::Assign(
                    replace_var(var, mapping)?,
                    replace_vars_in_expr(&expr, mapping)?,
                ),
                S::While { cond, body } => S::While {
                    cond: replace_vars_in_expr(cond, mapping)?,
                    body: replace_vars_in_block(block, mapping)?,
                },
                S::If { cond, then, or } => S::If {
                    cond: replace_vars_in_expr(cond, mapping)?,
                    then: replace_vars_in_block(then, mapping)?,
                    or: replace_vars_in_block(or, mapping)?,
                },
                S::Macro {
                    var,
                    prog_name,
                    input_expr,
                } => S::Macro {
                    var: replace_var(var, mapping)?,
                    prog_name: prog_name.clone(),
                    input_expr: replace_vars_in_expr(input_expr, mapping)?,
                },
                S::Switch {
                    cond,
                    cases,
                    default,
                } => S::Switch {
                    cond: replace_vars_in_expr(cond, mapping)?,
                    cases: cases
                        .into_iter()
                        .map(|(expr, block)| {
                            Ok((
                                replace_vars_in_expr(expr, mapping)?,
                                replace_vars_in_block(block, mapping)?,
                            ))
                        })
                        .collect::<anyhow::Result<_>>()?,
                    default: replace_vars_in_block(default, mapping)?,
                },
            })
        })
        .collect::<anyhow::Result<_>>()?;
    Ok(Block(new_stmts))
}

fn replace_vars_in_expr(
    expr: &Expression,
    mapping: &IndexMap<VarName, VarName>,
) -> anyhow::Result<Expression> {
    use Expression as E;
    Ok(match expr {
        E::Cons(e1, e2) => E::Cons(
            Box::new(replace_vars_in_expr(e1, mapping)?),
            Box::new(replace_vars_in_expr(e2, mapping)?),
        ),
        E::Hd(e) => E::Hd(Box::new(replace_vars_in_expr(e, mapping)?)),
        E::Tl(e) => E::Tl(Box::new(replace_vars_in_expr(e, mapping)?)),
        E::Nil => E::Nil,
        E::Var(var) => E::Var(replace_var(var, mapping)?),
        E::Num(n) => E::Num(*n),
        E::Bool(b) => E::Bool(*b),
        E::List(list) => E::List(
            list.into_iter()
                .map(|e| Ok(replace_vars_in_expr(e, mapping)?))
                .collect::<anyhow::Result<_>>()?,
        ),
        E::Eq(e1, e2) => E::Eq(
            Box::new(replace_vars_in_expr(e1, mapping)?),
            Box::new(replace_vars_in_expr(e2, mapping)?),
        ),
    })
}

fn replace_var(
    var_name: &VarName,
    mapping: &IndexMap<VarName, VarName>,
) -> anyhow::Result<VarName> {
    mapping
        .get(var_name)
        .map(|x| x.to_owned())
        .with_context(|| format!("Var name '{var_name}' did not exist in replacements map!"))
}

// equals is the worst case, as the equals expression has to be replaced with a new expression... and entirely new statement has to be added! Nested equals get particularly head-scratching to work out.

pub fn equals_to_core(_prog: &Prog) -> Prog {
    todo!()
}
