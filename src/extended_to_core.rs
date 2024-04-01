use anyhow::{bail, Context};
use indexmap::IndexMap;
use once_cell::sync::Lazy;

use crate::{
    lang::{Block, Expression, Prog, ProgName, Statement},
    niltree::NilTree,
    parser::parse,
    variables::{VarName, Variables},
};

pub fn prog_to_core(prog: &Prog, progs: &IndexMap<ProgName, Prog>) -> anyhow::Result<Prog> {
    let mut prog = prog.clone();
    // first convert all the macros over
    prog = macros_to_core(&prog, progs)?;
    // then convert everything but macros and equality to core
    prog.body = block_to_core(&prog.body);
    // then convert the equalities into equalG macros
    prog = equals_to_core(&prog);
    // then convert the equalG macros into core
    prog = macros_to_core(&prog, &EQUALG_PROGRAM)?;
    Ok(prog)
}

pub fn num_to_core(n: usize) -> Expression {
    let mut res = Expression::Nil;
    for _ in 0..n {
        res = Expression::Cons(Box::new(Expression::Nil), Box::new(res));
    }
    res
}

pub fn num_to_niltree(n: usize) -> NilTree {
    if n == 0 {
        NilTree::Nil
    } else {
        NilTree::List(vec![NilTree::Nil; n])
    }
}

pub fn list_to_cons(list: &[Expression]) -> Expression {
    match list {
        [] => Expression::Nil,
        v => Expression::Cons(Box::new(v[0].clone()), list_to_cons(&v[1..]).into()),
    }
}

pub fn list_to_core(list: &[Expression]) -> Expression {
    match list {
        [] => Expression::Nil,
        v => Expression::Cons(Box::new(expr_to_core(&v[0])), list_to_core(&v[1..]).into()),
    }
}

pub fn bool_to_core(b: bool) -> Expression {
    match b {
        true => Expression::Cons(Box::new(Expression::Nil), Box::new(Expression::Nil)),
        false => Expression::Nil,
    }
}

pub fn expr_to_core(expr: &Expression) -> Expression {
    use Expression as E;
    match expr {
        E::Cons(e1, e2) => E::Cons(Box::new(expr_to_core(e1)), Box::new(expr_to_core(e2))),
        E::Hd(e) => E::Hd(Box::new(expr_to_core(e))),
        E::Tl(e) => E::Tl(Box::new(expr_to_core(e))),
        E::Nil | E::Var(_) => expr.clone(),
        E::Num(n) => num_to_core(*n),
        E::Bool(b) => bool_to_core(*b),
        E::List(l) => list_to_core(l),
        E::Eq(e1, e2) => E::Eq(Box::new(expr_to_core(e1)), Box::new(expr_to_core(e2))),
    }
}

pub fn stmt_to_core(stmt: &Statement) -> Statement {
    use Statement as S;
    match stmt {
        S::Assign(v, e) => S::Assign(v.clone(), expr_to_core(e)),
        S::While { cond, body } => S::While {
            cond: expr_to_core(cond),
            body: block_to_core(body),
        },
        S::If { cond, then, or } => S::If {
            cond: expr_to_core(cond),
            then: block_to_core(then),
            or: block_to_core(or),
        },
        S::Macro {
            var,
            prog_name,
            input_expr,
        } => S::Macro {
            var: var.clone(),
            prog_name: prog_name.clone(),
            input_expr: expr_to_core(input_expr),
        },
        S::Switch {
            cond,
            cases,
            default,
        } => switch_to_core(cond, cases, default),
    }
}

pub fn block_to_core(block: &Block) -> Block {
    let stmts = block.0.iter().map(|stmt| stmt_to_core(stmt)).collect();
    Block(stmts)
}

pub fn switch_to_core(
    cond: &Expression,
    cases: &Vec<(Expression, Block)>,
    default: &Block,
) -> Statement {
    stmt_to_core(&switch_to_ifs(cond, cases, default))
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

const EQUALG_PROGRAM: Lazy<IndexMap<ProgName, Prog>> = Lazy::new(|| {
    [(
        ProgName("equalG".into()),
        parse(include_str!("../programs/equalG.while")).unwrap(),
    )]
    .into()
});

pub fn equals_to_core(prog: &Prog) -> Prog {
    let mut vars = Variables::new(prog);
    let mut prog = prog.clone();
    prog.body = replace_equals_in_block(&prog.body, &mut vars);
    prog
}

fn replace_equals_in_block(block: &Block, vars: &mut Variables) -> Block {
    let mut stmts = vec![];
    for stmt in &block.0 {
        use Statement as S;
        match stmt {
            S::Assign(var, expr) => {
                let (expr, new_stmts) = replace_equals_in_expr(expr, vars);
                stmts.extend(new_stmts.into_iter());
                stmts.push(S::Assign(var.clone(), expr))
            }
            Statement::While { cond, body } => {
                let (cond, new_stmts) = replace_equals_in_expr(cond, vars);
                stmts.extend(new_stmts.into_iter());
                let body = replace_equals_in_block(body, vars);
                stmts.push(S::While { cond, body })
            }
            Statement::If { cond, then, or } => {
                let (cond, new_stmts) = replace_equals_in_expr(cond, vars);
                stmts.extend(new_stmts.into_iter());
                let then = replace_equals_in_block(then, vars);
                let or = replace_equals_in_block(or, vars);
                stmts.push(S::If { cond, then, or })
            }
            Statement::Macro {
                var,
                prog_name,
                input_expr,
            } => {
                let (input_expr, new_stmts) = replace_equals_in_expr(input_expr, vars);
                stmts.extend(new_stmts.into_iter());
                stmts.push(S::Macro {
                    var: var.clone(),
                    prog_name: prog_name.clone(),
                    input_expr,
                })
            }
            Statement::Switch {
                cond,
                cases,
                default,
            } => {
                if let S::If { cond, then, or } = switch_to_ifs(cond, cases, default) {
                    let (cond, new_stmts) = replace_equals_in_expr(&cond, vars);
                    stmts.extend(new_stmts.into_iter());
                    let then = replace_equals_in_block(&then, vars);
                    let or = replace_equals_in_block(&or, vars);
                    stmts.push(S::If { cond, then, or })
                } else {
                    panic!("Switch to ifs returned a non-If statement, please report this bug!")
                }
            }
        }
    }

    Block(stmts)
}

fn replace_equals_in_expr(expr: &Expression, vars: &mut Variables) -> (Expression, Vec<Statement>) {
    use Expression as E;
    match expr {
        E::Cons(e1, e2) => {
            let (e1, mut new_stmts) = replace_equals_in_expr(e1, vars);
            let (e2, new_stmts2) = replace_equals_in_expr(e2, vars);
            new_stmts.extend(new_stmts2.into_iter());

            (E::Cons(Box::new(e1), Box::new(e2)), new_stmts)
        }
        E::Hd(e) => {
            let (expr, stmts) = replace_equals_in_expr(e, vars);
            (E::Hd(Box::new(expr)), stmts)
        }
        E::Tl(e) => {
            let (expr, stmts) = replace_equals_in_expr(e, vars);
            (E::Tl(Box::new(expr)), stmts)
        }
        E::Nil | E::Num(_) | E::Bool(_) | E::Var(_) => (expr.clone(), vec![]),
        Expression::List(l) => replace_equals_in_expr(&list_to_core(l), vars),
        Expression::Eq(e1, e2) => {
            let (e1, mut new_stmts) = replace_equals_in_expr(e1, vars);
            let (e2, new_stmts2) = replace_equals_in_expr(e2, vars);
            new_stmts.extend(new_stmts2.into_iter());
            let new_var = vars.issue_name(&VarName("eq".into()));
            let new_expr = E::Var(new_var.clone());
            new_stmts.push(Statement::Macro {
                var: new_var,
                prog_name: ProgName("equalG".into()),
                input_expr: expr_to_core(&E::List(vec![E::List(vec![e1, e2])])),
            });
            (new_expr, new_stmts)
        }
    }
}
