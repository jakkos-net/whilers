// This module contains functions for generating output strings (e.g. nil trees, encodings of nil trees, programs-as-data etc.)

use std::fmt::Display;

use anyhow::bail;
use indexmap::IndexMap;
use regex::{Captures, Regex};

use crate::{
    atoms::Atom,
    extended_to_core::prog_to_core,
    interpret::{interpret, ExecState},
    lang::{Prog, ProgName},
    niltree::NilTree,
    prog_as_data::unparse_core_prog,
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

#[derive(Default, Debug, serde::Serialize, serde::Deserialize)]
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
                generate_output_with_debug(&output_tree, &store, debug, format_list_ints)
            }
            OutputFormat::NestedListOfIntegers => {
                generate_output_with_debug(&output_tree, &store, debug, format_nest_list_ints)
            }
            OutputFormat::NestedListOfAtoms => {
                generate_output_with_debug(&output_tree, &store, debug, format_nest_list_atoms)
            }
            OutputFormat::ProgramAsData => match prog_to_core(main_prog, progs) {
                Ok(prog) => Output::Text(unparse_core_prog(&prog)),
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

pub fn parse_num(tree: &NilTree) -> anyhow::Result<usize> {
    Ok(match tree {
        NilTree::Nil => 0,
        NilTree::List(v) => {
            if v.iter().all(|x| matches!(x, NilTree::Nil)) {
                v.len()
            } else {
                bail!("NaN")
            }
        }
        NilTree::Num(n) => *n,
    })
}

pub fn parse_num_str(tree: &NilTree) -> String {
    match parse_num(tree) {
        Ok(n) => n.to_string(),
        Err(e) => e.to_string(),
    }
}

pub fn parse_num_or_atom_str(tree: &NilTree) -> String {
    match parse_num(tree) {
        Ok(n) => num_to_num_or_atom_str(n),
        Err(e) => e.to_string(),
    }
}

pub fn num_to_num_or_atom_str(n: usize) -> String {
    if n < u8::MAX as usize {
        if let Ok(atom) = Atom::try_from(n as u8) {
            return atom.to_string();
        }
    }

    n.to_string()
}

// there is probably a better way to do these functions with iterators
pub fn format_list_f(tree: &NilTree, f: impl Fn(&NilTree) -> String) -> String {
    let mut res = vec![];
    if let NilTree::List(v) = tree {
        v.iter().rev().for_each(|nt| res.push(f(&nt)))
    } else if let NilTree::Num(n) = tree {
        (0..*n).for_each(|_| res.push(f(&NilTree::Nil)))
    }
    format!("[{}]", res.join(","))
}

pub fn format_list_ints(tree: &NilTree) -> String {
    format_list_f(tree, parse_num_str)
}

pub fn format_nest_list_ints(tree: &NilTree) -> String {
    format_list_f(tree, |tree| {
        if let Ok(_) = parse_num(tree) {
            parse_num_str(tree)
        } else {
            format_nest_list_ints(tree)
        }
    })
}

pub fn format_nest_list_atoms(tree: &NilTree) -> String {
    let s = format_nest_list_ints(tree);
    let re = Regex::new(r"\[\s*(\d*)\s*,").unwrap();
    re.replace_all(&s, |x: &Captures<'_>| {
        format!(
            "[{},",
            num_to_num_or_atom_str(x[1].to_string().parse().unwrap())
        )
    })
    .to_string()
}
