// This module contains functions for generating output strings (e.g. nil trees, encodings of nil trees, programs-as-data etc.)

use std::{collections::HashMap, fmt::Display};

use anyhow::bail;
use indexmap::IndexMap;

use crate::{
    atoms::Atom,
    interpret::{interpret, ExecState},
    parser::{Block, Expression, NilTree, Prog, ProgName, Statement, VarName},
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
            OutputFormat::ProgramAsData => Output::Text(unparse_prog(main_prog)),
            OutputFormat::CoreWhile => Output::Text(main_prog.to_string()),
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
    let varnums = Varnums::new(prog);
    format!(
        "[{},\n\t{},\n{}]",
        varnums.get(&prog.input_var),
        unparse_block(&prog.body, &varnums),
        varnums.get(&prog.output_var)
    )
}

pub struct Varnums(HashMap<VarName, usize>);

impl Varnums {
    pub fn new(prog: &Prog) -> Self {
        let mut varnums = Self(Default::default());
        varnums.try_add(&prog.input_var);
        Self::block(&prog.body, &mut varnums);
        varnums
    }

    fn block(block: &Block, varnums: &mut Self) {
        for stmt in &block.0 {
            match stmt {
                Statement::Assign(var, expr) => {
                    varnums.try_add(var);
                    Self::expr(expr, varnums);
                }
                Statement::While { cond, body } => {
                    Self::expr(cond, varnums);
                    Self::block(body, varnums);
                }
                Statement::If { cond, then, or } => {
                    Self::expr(cond, varnums);
                    Self::block(then, varnums);
                    Self::block(or, varnums);
                }
                Statement::Macro {
                    var: _,
                    prog_name: _,
                    input_expr: _,
                } => todo!(),
                Statement::Switch {
                    cond: _,
                    cases: _,
                    default: _,
                } => todo!(),
            }
        }
    }

    fn expr(expr: &Expression, varnums: &mut Self) {
        match expr {
            Expression::Cons(e1, e2) => {
                Self::expr(e1, varnums);
                Self::expr(e2, varnums);
            }
            Expression::Hd(e) => Self::expr(e, varnums),
            Expression::Tl(e) => Self::expr(e, varnums),
            Expression::Var(var) => varnums.try_add(var),
            _ => (),
        }
    }

    pub fn try_add(&mut self, var: &VarName) {
        if !self.0.contains_key(var) {
            self.0.insert(var.clone(), self.0.len());
        }
    }

    pub fn get(&self, var: &VarName) -> usize {
        *self.0.get(var).unwrap()
    }
}

pub fn unparse_block(block: &Block, varnums: &Varnums) -> String {
    format!(
        "[\n\t{}\n]",
        block
            .0
            .iter()
            .map(|stmt| unparse_stmt(stmt, varnums))
            .collect::<Vec<_>>()
            .join(",\n")
    )
}

pub fn unparse_stmt(stmt: &Statement, varnums: &Varnums) -> String {
    format!(
        "[{}]",
        match stmt {
            Statement::Assign(var, expr) =>
                format!("@:=, {}, {}", varnums.get(var), unparse_expr(expr, varnums)),
            Statement::While { cond, body } => format!(
                "@while,{},\n\t{}",
                unparse_expr(cond, varnums),
                unparse_block(body, varnums)
            ),
            Statement::If { cond, then, or } => format!(
                "@if,{},\n\t{}{}",
                unparse_expr(cond, varnums),
                unparse_block(then, varnums),
                format!(",\n\t{}", unparse_block(or, varnums))
            ),
            _ => panic!("Cannot unparse extended While statment: {stmt}"),
        }
    )
}

pub fn unparse_expr(expr: &Expression, varnums: &Varnums) -> String {
    format!(
        "[{}]",
        match expr {
            Expression::Cons(e1, e2) => format!(
                "@cons, {}, {}",
                unparse_expr(e1, varnums),
                unparse_expr(e2, varnums)
            ),
            Expression::Hd(e) => format!("@hd, {}", unparse_expr(e, varnums)),
            Expression::Tl(e) => format!("@tl, {}", unparse_expr(e, varnums)),
            Expression::Nil => "@quote, nil".into(),
            Expression::Var(var) => format!("@var, {}", varnums.get(var)),
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
