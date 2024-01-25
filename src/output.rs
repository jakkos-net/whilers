// This module contains functions for generating output strings (e.g. nil trees, encodings of nil trees, programs-as-data etc.)

use std::fmt::Display;

use anyhow::bail;
use indexmap::{IndexMap, IndexSet};

use crate::{
    atoms::Atom,
    extended_to_core::prog_to_core,
    interpret::{interpret, ExecState},
    parser::{Block, Expression, NilTree, Prog, ProgName, Statement, VarName},
    utils::indent,
};

#[derive(PartialEq, Eq, Clone, serde::Serialize, serde::Deserialize, Copy)]
pub enum OutputFormat {
    NilTree,
    Integer,
    ListOfIntegers,
    NestedListOfIntegers,
    NestedListOfAtoms,
    ProgramAsData,
    CoreWhile,
}

impl OutputFormat {
    pub fn takes_input(&self) -> bool {
        match self {
            OutputFormat::NilTree => true,
            OutputFormat::Integer => true,
            OutputFormat::ListOfIntegers => true,
            OutputFormat::NestedListOfIntegers => true,
            OutputFormat::NestedListOfAtoms => true,
            OutputFormat::ProgramAsData => false,
            OutputFormat::CoreWhile => false,
        }
    }

    pub fn can_be_debugged(&self) -> bool {
        match self {
            OutputFormat::NilTree => true,
            OutputFormat::Integer => true,
            OutputFormat::ListOfIntegers => true,
            OutputFormat::NestedListOfIntegers => true,
            OutputFormat::NestedListOfAtoms => true,
            OutputFormat::ProgramAsData => false,
            OutputFormat::CoreWhile => false,
        }
    }
}

#[derive(Default, serde::Serialize, serde::Deserialize)]
pub enum Output {
    Text(String),
    Error(String),
    #[default]
    None,
}

impl Display for OutputFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OutputFormat::NilTree => "Nil Tree",
            OutputFormat::Integer => "Integer",
            OutputFormat::ListOfIntegers => "List of integers",
            OutputFormat::NestedListOfIntegers => "Nested list of integers",
            OutputFormat::NestedListOfAtoms => "Nested list of atoms",
            OutputFormat::ProgramAsData => "Program as data",
            OutputFormat::CoreWhile => "Core While",
        }
        .fmt(f)
    }
}

pub fn generate_output(
    main_prog: &Prog,
    input: &NilTree,
    progs: &IndexMap<ProgName, Prog>,
    format: &OutputFormat,
    debug: bool,
) -> Output {
    let res = interpret(main_prog, input, progs);

    match res {
        Ok((output_tree, store)) => match format {
            OutputFormat::NilTree => {
                generate_output_with_debug(&output_tree, &store, debug, |x| x.to_string())
            }
            OutputFormat::Integer => {
                generate_output_with_debug(&output_tree, &store, debug, parse_num_str)
            }
            OutputFormat::ListOfIntegers => {
                generate_output_with_debug(&output_tree, &store, debug, parse_list_ints)
            }
            OutputFormat::NestedListOfIntegers => {
                generate_output_with_debug(&output_tree, &store, debug, parse_nest_list_ints)
            }
            OutputFormat::NestedListOfAtoms => {
                generate_output_with_debug(&output_tree, &store, debug, parse_nest_list_atoms)
            }
            OutputFormat::ProgramAsData => match prog_to_core(main_prog, progs) {
                Ok(prog) => Output::Text(unparse_prog(&prog)),
                Err(e) => Output::Error(e.to_string()),
            },
            OutputFormat::CoreWhile => match prog_to_core(main_prog, progs) {
                Ok(prog) => Output::Text(prog.to_string()),
                Err(e) => Output::Error(e.to_string()),
            },
        },
        Err(e) => Output::Error(format!("Program failed to run!\n{e}")),
    }
}

fn generate_output_with_debug(
    output_tree: &NilTree,
    store: &ExecState,
    debug: bool,
    f: impl Fn(&NilTree) -> String,
) -> Output {
    let mut res = vec![];
    if debug {
        for (prog_name, var, val) in store.get_history() {
            res.push(format!("{prog_name} {var} = {}", f(val)));
        }
        res.push("".into());
    }

    res.push(f(output_tree));
    Output::Text(res.join("\n"))
}

pub fn unparse_prog(prog: &Prog) -> String {
    let vars = Variables::new(prog);
    format!(
        "[{}, {}, {}]",
        vars.get(&prog.input_var),
        unparse_block(&prog.body, &vars),
        vars.get(&prog.output_var)
    )
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

pub fn unparse_block(block: &Block, vars: &Variables) -> String {
    if block.0.is_empty() {
        return "[]".into();
    }
    format!(
        "[\n{}\n]",
        block
            .0
            .iter()
            .map(|stmt| indent(unparse_stmt(stmt, vars).as_str()))
            .collect::<Vec<_>>()
            .join(",\n")
    )
}

pub fn unparse_stmt(stmt: &Statement, vars: &Variables) -> String {
    format!(
        "[{}]",
        match stmt {
            Statement::Assign(var, expr) =>
                format!("@:=, {}, {}", vars.get(var), unparse_expr(expr, vars)),
            Statement::While { cond, body } => format!(
                "@while, {}, {}",
                unparse_expr(cond, vars),
                unparse_block(body, vars)
            ),
            Statement::If { cond, then, or } => format!(
                "@if, {}, {}, {}",
                unparse_expr(cond, vars),
                unparse_block(then, vars),
                unparse_block(or, vars)
            ),
            _ => panic!("Cannot unparse extended While statment: {stmt}"),
        }
    )
}

pub fn unparse_expr(expr: &Expression, vars: &Variables) -> String {
    format!(
        "[{}]",
        match expr {
            Expression::Cons(e1, e2) => format!(
                "@cons, {}, {}",
                unparse_expr(e1, vars),
                unparse_expr(e2, vars)
            ),
            Expression::Hd(e) => format!("@hd, {}", unparse_expr(e, vars)),
            Expression::Tl(e) => format!("@tl, {}", unparse_expr(e, vars)),
            Expression::Nil => "@quote, nil".into(),
            Expression::Var(var) => format!("@var, {}", vars.get(var)),
            _ => panic!("Cannot unparse extended While expression: {expr} "),
        }
    )
}

pub fn parse_num(tree: &NilTree) -> anyhow::Result<usize> {
    match tree {
        NilTree::Nil => Ok(0),
        NilTree::Node { left, right } if matches!(**left, NilTree::Nil) => {
            Ok(1 + parse_num(right)?)
        }
        _ => bail!("NaN"),
    }
}

pub fn parse_num_str(tree: &NilTree) -> String {
    match parse_num(tree) {
        Ok(n) => n.to_string(),
        Err(e) => e.to_string(),
    }
}

pub fn parse_num_or_atom_str(tree: &NilTree) -> String {
    match parse_num(tree) {
        Ok(n) => {
            if n < u8::MAX as usize {
                if let Ok(atom) = Atom::try_from(n as u8) {
                    return atom.to_string();
                }
            }

            n.to_string()
        }
        Err(e) => e.to_string(),
    }
}

// there is probably a better way to do these functions with iterators
pub fn parse_list_f(mut tree: &NilTree, f: impl Fn(&NilTree) -> String) -> String {
    let mut res = vec![];
    while let NilTree::Node { left, right } = tree {
        res.push(f(left));
        tree = right;
    }
    format!("[{}]", res.join(","))
}

pub fn parse_list_ints(tree: &NilTree) -> String {
    parse_list_f(tree, parse_num_str)
}

pub fn parse_nest_list_ints(tree: &NilTree) -> String {
    parse_list_f(tree, |tree| {
        if let Ok(_) = parse_num(tree) {
            parse_num_str(tree)
        } else {
            parse_nest_list_ints(tree)
        }
    })
}

pub fn parse_nest_list_atoms(tree: &NilTree) -> String {
    parse_list_f(tree, |tree| {
        if let Ok(_) = parse_num(tree) {
            parse_num_or_atom_str(tree)
        } else {
            parse_nest_list_atoms(tree)
        }
    })
}
