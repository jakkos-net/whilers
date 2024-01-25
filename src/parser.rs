// This module parses plain-text source code into While program ASTs

use std::{fmt::Display, str::FromStr};

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
    extended_to_core::{list_to_core, num_to_core},
    utils::indent,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Prog {
    pub prog_name: ProgName,
    pub input_var: VarName,
    pub body: Block,
    pub output_var: VarName,
}

impl Display for Prog {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        format!(
            "{} read {} {} write {}",
            self.prog_name, self.input_var, self.body, self.output_var
        )
        .fmt(f)
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        format!(
            "{{\n{}\n}}",
            self.0
                .iter()
                .map(|s| indent(s.to_string().as_str()))
                .collect::<Vec<_>>()
                .join(";\n")
        )
        .fmt(f)
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Assign(var, e) => format!("{var} := {e}"),
            Statement::While { cond, body } => format!("while {cond} {body}"),
            Statement::If { cond, then, or } => {
                format!(
                    "if {cond} {then}{}",
                    // skip the else clause if it's empty
                    if or.0.is_empty() {
                        "".into()
                    } else {
                        format!(" else {or}")
                    }
                )
            }
            Statement::Macro {
                var,
                prog_name: prog,
                input_expr: input,
            } => format!("{var} := <{prog}> {input}"),
            Statement::Switch {
                cond,
                cases,
                default,
            } => {
                let cases = cases
                    .iter()
                    .map(|(case, body)| {
                        let statements = body
                            .0
                            .iter()
                            .map(|stmt| stmt.to_string())
                            .collect::<Vec<_>>()
                            .join(";\n");
                        format!("case {case}:\n{statements}")
                    })
                    .collect::<Vec<_>>()
                    .join("\n");
                let default = format!(
                    "default:\n{}",
                    default
                        .0
                        .iter()
                        .map(|stmt| stmt.to_string())
                        .collect::<Vec<_>>()
                        .join(";\n")
                );
                format!("switch {cond} {{\n{cases}\n{default}}}")
            }
        }
        .fmt(f)
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Cons(e1, e2) => format!("cons {e1} {e2}"),
            Expression::Hd(e) => format!("hd {e}"),
            Expression::Tl(e) => format!("tl {e}"),
            Expression::Nil => "nil".into(),
            Expression::Var(var) => var.to_string(),
            Expression::Num(n) => n.to_string(),
            Expression::Bool(b) => b.to_string(),
            Expression::List(v) => format!(
                "[{}]",
                v.iter()
                    .map(|expr| expr.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Expression::Eq(a, b) => format!("{a} = {b}"),
        }
        .fmt(f)
    }
}
#[derive(Debug, PartialEq, Eq, Clone, std::hash::Hash)]
pub struct VarName(pub String);

impl Display for VarName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, std::hash::Hash)]
pub struct ProgName(pub String);

impl Display for ProgName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Block(pub Vec<Statement>);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    Assign(VarName, Expression),
    While {
        cond: Expression,
        body: Block,
    },
    If {
        cond: Expression,
        then: Block,
        or: Block,
    },
    Macro {
        var: VarName,
        prog_name: ProgName,
        input_expr: Expression,
    },
    Switch {
        cond: Expression,
        cases: Vec<(Expression, Block)>,
        default: Block,
    },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    Cons(Box<Expression>, Box<Expression>),
    Hd(Box<Expression>),
    Tl(Box<Expression>),
    Nil,
    Var(VarName),
    Num(usize),
    Bool(bool),
    List(Vec<Expression>),
    Eq(Box<Expression>, Box<Expression>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum NilTree {
    Nil,
    Node {
        left: Box<NilTree>,
        right: Box<NilTree>,
    },
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
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
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
            delimited(multispace0, tag("}"), multispace0),
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
    alt((
        // Assignment
        map(
            pair(
                var_name,
                preceded(delimited(multispace0, tag(":="), multispace0), expression),
            ),
            |(var_name, expression)| Statement::Assign(var_name, expression),
        ),
        // While
        map(
            preceded(
                tag("while"),
                pair(delimited(multispace1, expression, multispace1), block),
            ),
            |(cond, body)| Statement::While { cond, body },
        ),
        // If Else
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
        ),
        // Switch
        map(
            preceded(
                tag("switch"),
                tuple((
                    // the condition
                    delimited(multispace1, expression, multispace0),
                    delimited(
                        pair(tag("{"), multispace0),
                        pair(
                            // list of cases
                            separated_list0(
                                multispace1,
                                pair(
                                    delimited(
                                        tag("case"),
                                        delimited(multispace1, expression, multispace0),
                                        tag(":"),
                                    ),
                                    statement_list,
                                ),
                            ),
                            // an optional default
                            opt(preceded(
                                tuple((multispace1, tag("default"), multispace0, tag(":"))),
                                statement_list,
                            )),
                        ),
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
        ),
        // Macro
        map(
            separated_pair(
                var_name,
                delimited(multispace0, tag(":="), multispace0),
                separated_pair(
                    delimited(tag("<"), prog_name, tag(">")),
                    multispace1,
                    expression,
                ),
            ),
            |(var, (prog, input))| Statement::Macro {
                var,
                prog_name: prog,
                input_expr: input,
            },
        ),
    ))(s)
}

// The grammar for equals is left-recursive: EXPR -> EXPR = EXPR
// this would lead to an infinite loop.
// So we break up the expression into expression and not_equals_expression
// and define equals as EXPR -> NOT_EQUALS_EXPR = EXPR
pub fn expression(s: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    alt((
        // Equals
        map(
            separated_pair(
                non_equality_expression,
                delimited(multispace0, tag("="), multispace0),
                expression,
            ),
            |(e1, e2)| Expression::Eq(Box::new(e1), Box::new(e2)),
        ),
        non_equality_expression,
    ))(s)
}

pub fn non_equality_expression(s: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    alt((
        // Hd
        map(preceded(pair(tag("hd"), multispace1), expression), |e| {
            Expression::Hd(e.into())
        }),
        // Tl
        map(preceded(pair(tag("tl"), multispace1), expression), |e| {
            Expression::Tl(e.into())
        }),
        // Cons
        map(
            preceded(
                tag("cons"),
                pair(
                    preceded(multispace1, expression),
                    preceded(multispace1, expression),
                ),
            ),
            |(e1, e2)| Expression::Cons(e1.into(), e2.into()),
        ),
        // Lists
        map(
            delimited(
                tag("["),
                separated_list0(tag(","), delimited(multispace0, expression, multispace0)),
                tag("]"),
            ),
            |v| list_to_core(&v[..]),
        ),
        // Brackets
        map(
            delimited(
                pair(tag("("), multispace0),
                expression,
                pair(multispace0, tag(")")),
            ),
            |expr| expr,
        ),
        // Nil
        map(tag("nil"), |_| Expression::Nil),
        // Bools
        map(tag("true"), |_| {
            Expression::Cons(Expression::Nil.into(), Expression::Nil.into())
        }),
        map(tag("false"), |_| Expression::Nil),
        // Atoms
        map_res(preceded(tag("@"), alt((alpha1, tag(":=")))), |atom| {
            Atom::from_str(format!("@{atom}").as_str()).map(|atom| num_to_core(atom as u8 as usize))
        }),
        // Numbers
        map(digit1, |n: &str| Expression::Num(n.parse().unwrap())),
        // Variables
        map(var_name, Expression::Var),
    ))(s)
}

#[cfg(test)]
mod tests {
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
}
