// This module parses plain-text source code into While program ASTs

use std::str::FromStr;

use anyhow::bail;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::{
        complete::multispace0,
        complete::{alpha1, alphanumeric1, digit1, multispace1},
    },
    combinator::{eof, map, map_res, opt, recognize},
    error::{convert_error, VerboseError},
    multi::{many0_count, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};
use regex::Regex;

use crate::{
    atoms::Atom,
    lang::{Block, Expression, Prog, ProgName, Statement},
    variables::VarName,
};

pub fn parse(s: &str) -> anyhow::Result<Prog> {
    let s = remove_comments(s);
    match prog(&s) {
        Ok((_, prog)) => Ok(prog),
        Err(e) => match e {
            nom::Err::Error(e) | nom::Err::Failure(e) => {
                let e = convert_error(s.as_str(), e);
                bail!("failed to parse program:\n{e}")
            }
            _ => bail!("Unknown error!"),
        },
    }
}

fn prog(s: &str) -> IResult<&str, Prog, VerboseError<&str>> {
    map(
        tuple((
            delimited(
                multispace0,
                prog_name,
                delimited(multispace1, tag("read"), multispace1),
            ),
            var_name,
            terminated(delimited(multispace0, block, multispace0), tag("write")),
            terminated(delimited(multispace1, var_name, multispace0), eof),
        )),
        |(name, input, body, output)| Prog {
            prog_name: name,
            input_var: input,
            body,
            output_var: output,
        },
    )(s)
}

pub fn get_prog_name_string_fast(s: &str) -> String {
    let s = remove_comments(s);

    let mut parser = delimited(
        multispace0,
        prog_name,
        delimited(multispace1, tag("read"), multispace1),
    );

    let res = parser(&s);

    if let Ok((_, prog_name)) = res {
        prog_name.to_string()
    } else {
        "?".into()
    }
}

pub fn remove_comments(s: &str) -> String {
    let single_line = Regex::new(r"//.*").unwrap();
    let multi_line = Regex::new(r"\(\*(.|\n)*?\*\)").unwrap();
    multi_line
        .replace_all(&single_line.replace_all(s, "\n"), "")
        .to_string()
}

pub fn name(s: &str) -> IResult<&str, &str, VerboseError<&str>> {
    recognize(pair(
        alt((alpha1, tag("_"), tag("'"))),
        many0_count(alt((alphanumeric1, tag("_"), tag("'")))),
    ))(s)
}

pub fn prog_name(s: &str) -> IResult<&str, ProgName, VerboseError<&str>> {
    name(s).map(|(s, name)| (s, ProgName(name.into())))
}

pub fn var_name(s: &str) -> IResult<&str, VarName, VerboseError<&str>> {
    name(s).map(|(s, name)| (s, VarName(name.into())))
}

pub fn block(s: &str) -> IResult<&str, Block, VerboseError<&str>> {
    map(
        delimited(
            delimited(multispace0, tag("{"), multispace0),
            statement_list,
            preceded(multispace0, tag("}")),
        ),
        Block,
    )(s)
}

pub fn statement_list(s: &str) -> IResult<&str, Vec<Statement>, VerboseError<&str>> {
    separated_list0(
        delimited(multispace0, tag(";"), multispace0),
        preceded(multispace0, statement),
    )(s)
}

pub fn statement(s: &str) -> IResult<&str, Statement, VerboseError<&str>> {
    alt((assign_stmt, while_stmt, if_stmt, switch_stmt, macro_stmt))(s)
}

fn assign_stmt(s: &str) -> IResult<&str, Statement, VerboseError<&str>> {
    map(
        pair(
            var_name,
            preceded(delimited(multispace0, tag(":="), multispace0), expression),
        ),
        |(var_name, expression)| Statement::Assign(var_name, expression),
    )(s)
}

fn while_stmt(s: &str) -> IResult<&str, Statement, VerboseError<&str>> {
    map(
        preceded(
            tag("while"),
            pair(delimited(multispace1, expression, multispace1), block),
        ),
        |(cond, body)| Statement::While { cond, body },
    )(s)
}

fn if_stmt(s: &str) -> IResult<&str, Statement, VerboseError<&str>> {
    map(
        preceded(
            tag("if"),
            tuple((
                delimited(multispace1, expression, multispace1),
                block,
                opt(preceded(
                    delimited(multispace0, tag("else"), multispace0),
                    block,
                )),
            )),
        ),
        |(cond, then, or)| Statement::If {
            cond,
            then,
            or: or.unwrap_or(Block(vec![])),
        },
    )(s)
}

fn switch_stmt(s: &str) -> IResult<&str, Statement, VerboseError<&str>> {
    map(
        preceded(
            tag("switch"),
            tuple((
                // the condition
                delimited(multispace1, expression, multispace0),
                delimited(
                    pair(tag("{"), multispace0),
                    switch_case_list,
                    pair(multispace0, tag("}")),
                ),
            )),
        ),
        |(cond, (cases, default))| Statement::Switch {
            cond,
            cases: cases
                .into_iter()
                .map(|(expr, stmts)| (expr, Block(stmts)))
                .collect::<Vec<_>>(),
            default: default.map(Block).unwrap_or(Block(vec![])),
        },
    )(s)
}

fn switch_case_list(
    s: &str,
) -> IResult<&str, (Vec<(Expression, Vec<Statement>)>, Option<Vec<Statement>>), VerboseError<&str>>
{
    pair(
        // list of cases
        separated_list0(multispace1, switch_case),
        // an optional default
        opt(preceded(
            tuple((multispace1, tag("default"), multispace0, tag(":"))),
            statement_list,
        )),
    )(s)
}

fn switch_case(s: &str) -> IResult<&str, (Expression, Vec<Statement>), VerboseError<&str>> {
    pair(
        delimited(
            tag("case"),
            delimited(multispace1, expression, multispace0),
            tag(":"),
        ),
        statement_list,
    )(s)
}

fn macro_stmt(s: &str) -> IResult<&str, Statement, VerboseError<&str>> {
    map(
        separated_pair(
            var_name,
            delimited(multispace0, tag(":="), multispace0),
            separated_pair(
                delimited(tag("<"), prog_name, tag(">")),
                multispace0,
                expression,
            ),
        ),
        |(var, (prog, input))| Statement::Macro {
            var,
            prog_name: prog,
            input_expr: input,
        },
    )(s)
}

// The grammar for equals is left-recursive: EXPR -> EXPR = EXPR
// this would lead to an infinite loop.
// So we break up the expression into expression and not_equals_expression
// and define equals as EXPR -> NOT_EQUALS_EXPR = EXPR
pub fn expression(s: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    // we know that we need an expression
    let (s, expr) = non_equality_expression(s)?;

    // now check if after that expr we have an "=" and then another expr
    if let Ok((s, other_expr)) =
        preceded(tuple((multispace0, tag("="), multispace0)), expression)(s)
    {
        Ok((s, Expression::Eq(Box::new(expr), Box::new(other_expr))))
    } else {
        Ok((s, expr))
    }
}

pub fn non_equality_expression(s: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    alt((
        brackets_expr,
        hd_expr,
        tl_expr,
        cons_expr,
        num_expr,
        list_expr,
        map(tag("nil"), |_| Expression::Nil),
        map(tag("true"), |_| {
            Expression::Cons(Expression::Nil.into(), Expression::Nil.into())
        }),
        map(tag("false"), |_| Expression::Nil),
        map(var_name, Expression::Var),
        tree_literal_expr,
        atom_expr,
    ))(s)
}

fn hd_expr(s: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    map(preceded(pair(tag("hd"), multispace1), expression), |e| {
        Expression::Hd(e.into())
    })(s)
}

fn tl_expr(s: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    map(preceded(pair(tag("tl"), multispace1), expression), |e| {
        Expression::Tl(e.into())
    })(s)
}

fn cons_expr(s: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    map(
        preceded(
            tag("cons"),
            pair(
                preceded(multispace1, expression),
                preceded(multispace1, expression),
            ),
        ),
        |(e1, e2)| Expression::Cons(e1.into(), e2.into()),
    )(s)
}

fn num_expr(s: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    map(digit1, |n: &str| Expression::Num(n.parse().unwrap()))(s)
}

fn list_expr(s: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    map(
        delimited(
            terminated(tag("["), multispace0),
            separated_list0(tag(","), delimited(multispace0, expression, multispace0)),
            preceded(multispace0, tag("]")),
        ),
        |v| Expression::List(v),
    )(s)
}

fn brackets_expr(s: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    map(
        delimited(
            pair(tag("("), multispace0),
            expression,
            pair(multispace0, tag(")")),
        ),
        |expr| expr,
    )(s)
}

fn atom_expr(s: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    map_res(preceded(tag("@"), alt((alpha1, tag(":=")))), |atom| {
        Atom::from_str(format!("@{atom}").as_str()).map(|atom| Expression::Num(atom as usize))
    })(s)
}

fn tree_literal_expr(s: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    alt((
        map(tag("nil"), |_| Expression::Nil),
        map(
            tuple((
                tag("<"),
                tree_literal_expr,
                tag("."),
                tree_literal_expr,
                tag(">"),
            )),
            |(_, l, _, r, _)| Expression::Cons(Box::new(l), Box::new(r)),
        ),
    ))(s)
}

#[cfg(test)]
mod tests {
    use crate::extended_to_core::num_to_niltree;
    use crate::parser::Expression;
    use crate::parser::{parse, Block, Prog, ProgName, VarName};

    use super::Expression::*;
    use super::Statement::*;
    #[test]
    fn test_prog() {
        let s = include_str!("../programs/add.while");
        let prog = parse(s).unwrap();

        assert_eq!(
            prog,
            Prog {
                prog_name: ProgName("add".into()),
                input_var: VarName("XY".into()),
                body: Block(vec![
                    Assign(VarName("X".into()), Hd(Var(VarName("XY".into())).into())),
                    Assign(
                        VarName("Y".into()),
                        Hd(Tl(Var(VarName("XY".into())).into()).into())
                    ),
                    While {
                        cond: Var(VarName("X".into())),
                        body: Block(vec![
                            Assign(
                                VarName("Y".into()),
                                Cons(Nil.into(), Var(VarName("Y".into())).into())
                            ),
                            Assign(VarName("X".into()), Tl(Var(VarName("X".into())).into()))
                        ])
                    }
                ]),
                output_var: VarName("Y".into())
            }
        )
    }

    #[test]
    fn test_switch1() {
        let s = include_str!("../programs/switch1.while");
        let prog = parse(s).unwrap();

        assert_eq!(
            prog,
            Prog {
                prog_name: ProgName("switch1".into()),
                input_var: VarName("X".into()),
                body: Block(vec![Switch {
                    cond: Var(VarName("X".into())),
                    cases: vec![
                        (Num(3), Block(vec![Assign(VarName("Y".into()), Num(3))])),
                        (Num(4), Block(vec![Assign(VarName("Y".into()), Num(4))])),
                        (
                            Var(VarName("doFor".into())),
                            Block(vec![Assign(VarName("Y".into()), Num(1000))])
                        )
                    ],
                    default: Block(vec![])
                }]),
                output_var: VarName("Y".into())
            }
        )
    }

    #[test]
    fn test_switch2() {
        let s = include_str!("../programs/switch2.while");
        let prog = parse(s).unwrap();

        assert_eq!(
            prog,
            Prog {
                prog_name: ProgName("switch2".into()),
                input_var: VarName("X".into()),
                body: Block(vec![Switch {
                    cond: Var(VarName("X".into())),
                    cases: vec![
                        (Num(3), Block(vec![Assign(VarName("Y".into()), Num(3))])),
                        (
                            Num(4),
                            Block(vec![
                                Assign(VarName("Y".into()), Num(4)),
                                Assign(VarName("Z".into()), Num(5))
                            ])
                        ),
                        (
                            Var(VarName("doFor".into())),
                            Block(vec![
                                Assign(VarName("Y".into()), Num(1000)),
                                Assign(VarName("Z".into()), Num(7))
                            ])
                        )
                    ],
                    default: Block(vec![Assign(VarName("Y".into()), Num(137))])
                }]),
                output_var: VarName("Y".into())
            }
        )
    }
    #[test]
    fn test_switch3() {
        let s = include_str!("../programs/switch3.while");
        let prog = parse(s).unwrap();

        assert_eq!(
            prog,
            Prog {
                prog_name: ProgName("switch3".into()),
                input_var: VarName("X".into()),
                body: Block(vec![Switch {
                    cond: Var(VarName("X".into())),
                    cases: vec![
                        (Num(3), Block(vec![Assign(VarName("Y".into()), Num(3))])),
                        (
                            Num(4),
                            Block(vec![
                                Assign(VarName("Y".into()), Num(4)),
                                Assign(VarName("Z".into()), Num(5))
                            ])
                        ),
                        (
                            Var(VarName("doFor".into())),
                            Block(vec![
                                Assign(VarName("Y".into()), Num(1000)),
                                Assign(VarName("Z".into()), Num(7))
                            ])
                        ),
                        (
                            Num(5),
                            Block(vec![If {
                                cond: Expression::Cons(
                                    Box::new(Expression::Nil),
                                    Box::new(Expression::Nil)
                                ),
                                then: Block(vec![Assign(VarName("Y".into()), Num(9))]),
                                or: Block(vec![])
                            }])
                        )
                    ],
                    default: Block(vec![Assign(VarName("Y".into()), Num(137))])
                }]),
                output_var: VarName("Y".into())
            }
        )
    }

    // we had a problem with lots of brackets leading to very slow parsing
    #[test]
    fn test_brackets() {
        let s = include_str!("../programs/brackets.while");
        let _ = parse(s).unwrap();
    }

    #[test]
    fn test_stack_overflow() {
        let n = 1_000_000;
        let n = num_to_niltree(n);
        let m = n.clone();
        let _ = m == n;
    }
}
