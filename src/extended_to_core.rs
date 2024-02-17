// This module contains functionality for converting Extended While programs into Core While programs.

use anyhow::{bail, Context};
use indexmap::IndexMap;
use once_cell::sync::Lazy;

use crate::{
    output::Variables,
    parser::{parse, Block, Expression, Prog, ProgName, Statement, VarName},
};

pub fn prog_to_core(prog: &Prog, progs: &IndexMap<ProgName, Prog>) -> anyhow::Result<Prog> {
    let prog = macros_to_core(prog, progs);

    prog
}
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

fn macros_to_core(prog: &Prog, progs: &IndexMap<ProgName, Prog>) -> anyhow::Result<Prog> {
    fn convert<'a>(block: &Block, context: &mut Context<'a>) -> anyhow::Result<Block> {
        // we only want to replace one macro at a time
        if context.made_change {
            return Ok(block.clone());
        }

        let mut stmt_list = vec![];

        for stmt in &block.0 {
            use Statement as S;
            match stmt {
                S::Assign(_, _) => stmt_list.push(stmt.clone()),
                Statement::While { cond, body } => stmt_list.push(S::While {
                    cond: cond.clone(),
                    body: convert(body, context)?,
                }),
                Statement::If { cond, then, or } => stmt_list.push(S::If {
                    cond: cond.clone(),
                    then: convert(then, context)?,
                    or: convert(or, context)?,
                }),
                Statement::Switch {
                    cond,
                    cases,
                    default,
                } => stmt_list.push(S::Switch {
                    cond: cond.clone(),
                    cases: cases
                        .into_iter()
                        .map(|(cond, block)| Ok((cond.clone(), convert(block, context)?)))
                        .collect::<anyhow::Result<_>>()?,
                    default: convert(default, context)?,
                }),
                Statement::Macro {
                    var,
                    prog_name,
                    input_expr,
                } => {
                    context.made_change = true;

                    let macro_prog = context.progs.get(prog_name).with_context(|| format!("Tried to convert macro call to '{prog_name}' to core while, but this program does not exist!"))?;
                    let macro_vars = Variables::new(macro_prog);

                    let mapping = macro_vars
                        .iter()
                        .map(|name| (name.clone(), context.main_prog_vars.issue_name(name)))
                        .collect();

                    stmt_list.push(S::Assign(
                        replace_var(&macro_prog.input_var, &mapping)?,
                        input_expr.clone(),
                    ));

                    stmt_list.extend(
                        replace_vars_in_block(&macro_prog.body, &mapping)?
                            .0
                            .into_iter(),
                    );

                    stmt_list.push(S::Assign(
                        var.clone(),
                        Expression::Var(replace_var(&macro_prog.output_var, &mapping)?),
                    ))
                }
            };
        }
        Ok(Block(stmt_list))
    }

    struct Context<'a> {
        main_prog_vars: Variables,
        progs: &'a IndexMap<ProgName, Prog>,
        made_change: bool,
    }

    let max_iter = 1000;
    let mut i = 0;
    let mut prog = prog.clone();
    loop {
        let mut context = Context {
            main_prog_vars: Variables::new(&prog),
            progs,
            made_change: false,
        };

        prog.body = convert(&prog.body, &mut context)?;

        // if we didn't convert any macros during a pass, it means we are finished
        if !context.made_change {
            break;
        }

        i += 1;
        if i >= max_iter {
            bail!("Exceeded max iteration count when trying to convert macro calls to core While, check if the program contains recursive macro calls (which are not allowed).")
        }
    }

    Ok(prog)
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
                    body: replace_vars_in_block(body, mapping)?,
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

const EQUALG_PROGRAM: Lazy<Prog> =
    Lazy::new(|| parse(include_str!("../programs/equalG.while")).unwrap());

pub fn equals_to_core(_prog: &Prog) -> Prog {
    todo!()
}
