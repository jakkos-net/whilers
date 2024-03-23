use crate::lang::{Block, Expression, Prog, Statement};
use indexmap::IndexSet;
use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone, std::hash::Hash)]
pub struct VarName(pub String);

impl Display for VarName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

pub struct Variables(IndexSet<VarName>);

impl Variables {
    pub fn new(prog: &Prog) -> Self {
        let mut vars = Self(Default::default());
        vars.add(&prog.input_var);
        vars.add_block(&prog.body);
        vars.add(&prog.output_var);
        vars
    }

    fn add_block(&mut self, block: &Block) {
        for stmt in &block.0 {
            match stmt {
                Statement::Assign(var, expr) => {
                    self.add(var);
                    self.add_expr(expr);
                }
                Statement::While { cond, body } => {
                    self.add_expr(cond);
                    self.add_block(body);
                }
                Statement::If { cond, then, or } => {
                    self.add_expr(cond);
                    self.add_block(then);
                    self.add_block(or);
                }
                Statement::Macro {
                    var,
                    prog_name: _,
                    input_expr,
                } => {
                    self.add(var);
                    self.add_expr(input_expr)
                }
                Statement::Switch {
                    cond,
                    cases,
                    default,
                } => {
                    self.add_expr(cond);
                    for (e, b) in cases {
                        self.add_expr(e);
                        self.add_block(b);
                    }
                    self.add_block(default);
                }
            }
        }
    }

    fn add_expr(&mut self, expr: &Expression) {
        use Expression as E;
        match expr {
            E::Cons(e1, e2) | E::Eq(e1, e2) => {
                self.add_expr(e1);
                self.add_expr(e2);
            }
            E::Hd(e) | E::Tl(e) => self.add_expr(e),
            E::Var(var) => self.add(var),
            E::Nil | E::Num(_) | E::Bool(_) => (),
            E::List(list) => {
                for e in list {
                    self.add_expr(e)
                }
            }
        }
    }

    pub fn add(&mut self, var: &VarName) {
        if !self.0.contains(var) {
            self.0.insert(var.clone());
        }
    }

    pub fn get(&self, var: &VarName) -> usize {
        self.0.get_index_of(var).unwrap()
    }

    pub fn issue_name(&mut self, desired_name: &VarName) -> VarName {
        if self.0.contains(desired_name) {
            let alternative_name = VarName(format!("{desired_name}'"));
            self.issue_name(&alternative_name)
        } else {
            self.add(desired_name);
            desired_name.clone()
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &VarName> {
        self.0.iter()
    }
}
