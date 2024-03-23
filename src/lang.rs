use crate::utils::indent;
use crate::variables::VarName;
use std::fmt::Display;

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
