// This module contains functions for executing While programs

use std::collections::HashMap;

use anyhow::{bail, Context};
use indexmap::{IndexMap, IndexSet};

use crate::{
    extended_to_core::{list_to_core, num_to_core, switch_to_ifs},
    parser::{expression, Block, Expression, NilTree, Prog, ProgName, Statement, VarName},
};

const MAX_EXEC_STEPS: u32 = 1_000_000;

pub fn interpret(
    main_prog: &Prog,
    input: &NilTree,
    progs: &IndexMap<ProgName, Prog>,
) -> anyhow::Result<(NilTree, Store)> {
    let mut store: Store = Default::default();

    // keep track of which program we are inside, used for debugging information
    let _ = store.push_macro(&main_prog.prog_name)?;

    store.set(&main_prog.input_var, input);
    let mut exec_steps = 0;
    exec_block(&main_prog.body, &mut store, progs, &mut exec_steps)?;

    Ok((store.get(&main_prog.output_var).clone(), store))
}

fn exec_block(
    block: &Block,
    store: &mut Store,
    progs: &IndexMap<ProgName, Prog>,
    exec_steps: &mut u32,
) -> anyhow::Result<()> {
    for stmt in &block.0 {
        exec(stmt, store, progs, exec_steps)?
    }

    Ok(())
}

fn exec(
    stmt: &Statement,
    store: &mut Store,
    progs: &IndexMap<ProgName, Prog>,
    exec_steps: &mut u32,
) -> anyhow::Result<()> {
    *exec_steps += 1;
    if *exec_steps > MAX_EXEC_STEPS {
        bail!("Exceeded max execution steps, program terminated before completion!");
    }
    match stmt {
        Statement::Assign(var, expr) => store.set(var, &eval(expr, store)),
        Statement::While { cond, body } => {
            while eval(cond, store) != NilTree::Nil {
                exec_block(body, store, progs, exec_steps)?
            }
        }
        Statement::If { cond, then, or } => {
            let cond = eval(cond, store);
            match cond {
                NilTree::Nil => exec_block(or, store, progs, exec_steps)?,
                _ => exec_block(then, store, progs, exec_steps)?,
            };
        }
        Statement::Macro {
            var,
            prog_name,
            input_expr,
        } => {
            let macro_prog = progs.get(prog_name).with_context(|| {
                format!("Called macro '{prog_name}', but this program does not exist!")
            })?;
            let input_val = eval(&input_expr, store);
            let (output_value, macro_store) = interpret(&macro_prog, &input_val, progs)?;

            let _ = store.push_macro(prog_name)?;
            for change in macro_store.change_history {
                store.change_history.push(change.clone())
            }
            let _ = store.pop_macro()?;

            store.set(var, &output_value);
        }
        Statement::Switch {
            cond,
            cases,
            default,
        } => exec(
            &switch_to_ifs(cond, cases, default),
            store,
            progs,
            exec_steps,
        )?,
    }

    Ok(())
}

fn eval(expr: &Expression, store: &Store) -> NilTree {
    match expr {
        Expression::Cons(e1, e2) => NilTree::Node {
            left: eval(e1, store).into(),
            right: eval(e2, store).into(),
        },
        Expression::Hd(e) => match eval(e, store) {
            NilTree::Nil => NilTree::Nil,
            NilTree::Node { left, right: _ } => *left,
        },
        Expression::Tl(e) => match eval(e, store) {
            NilTree::Nil => NilTree::Nil,
            NilTree::Node { left: _, right } => *right,
        },
        Expression::Nil => NilTree::Nil,
        Expression::Var(var) => store.get(var).clone(),
        Expression::Num(n) => eval(&num_to_core(*n), store),
        Expression::Bool(b) => match b {
            true => NilTree::Node {
                left: Box::new(NilTree::Nil),
                right: Box::new(NilTree::Nil),
            },
            false => NilTree::Nil,
        },
        Expression::List(v) => eval(&list_to_core(&v[..]), store),
        Expression::Eq(a, b) => eval(&Expression::Bool(eval(a, store) == eval(b, store)), store),
    }
}

#[derive(Default)]
pub struct Store {
    store: HashMap<VarName, NilTree>,
    macro_stack: IndexSet<ProgName>,
    change_history: Vec<(Option<ProgName>, VarName, NilTree)>,
}

impl Store {
    pub fn set(&mut self, var: &VarName, data: &NilTree) {
        self.store.insert(var.clone(), data.clone());
        self.change_history
            .push((self.macro_stack.last().cloned(), var.clone(), data.clone()));
    }

    pub fn get(&self, var: &VarName) -> &NilTree {
        self.store.get(var).unwrap_or(&NilTree::Nil)
    }

    pub fn get_history(&self) -> &Vec<(Option<ProgName>, VarName, NilTree)> {
        &self.change_history
    }

    pub fn push_macro(&mut self, prog_name: &ProgName) -> anyhow::Result<()> {
        if self.macro_stack.contains(prog_name) {
            bail!("Tried to call macro '{prog_name}', however we are already inside a macro call '{prog_name}', this is not allowed!");
        }

        self.macro_stack.insert(prog_name.clone());

        Ok(())
    }

    pub fn pop_macro(&mut self) -> anyhow::Result<()> {
        if self.macro_stack.is_empty() {
            bail!("Tried to pop macro stack, but it is empty! This is a bug, please report this.");
        } else {
            self.macro_stack.pop();
        }

        Ok(())
    }
}

pub fn input(s: &str) -> anyhow::Result<NilTree> {
    match expression(s) {
        Ok((_, expr)) => Ok(eval(&expr, &Default::default())),
        Err(e) => bail!("Failed to parse input:\n{:?}", e),
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        interpret::{eval, input},
        parser::{expression, parse},
    };

    use super::interpret;

    #[test]
    pub fn test_add() {
        let prog = parse(include_str!("../programs/add.while")).unwrap();

        let input = input("[3,4]").unwrap();

        let progs = Default::default();

        let output = interpret(&prog, &input, &progs);

        let empty_store = Default::default();

        assert_eq!(
            output.unwrap().0,
            eval(&expression("7").unwrap().1, &empty_store)
        );
    }

    #[test]
    pub fn test_switch() {
        let prog = parse(include_str!("../programs/swTest.while")).unwrap();

        let progs = Default::default();

        let empty_store = Default::default();

        assert_eq!(
            interpret(&prog, &input("3").unwrap(), &progs).unwrap().0,
            eval(&expression("3").unwrap().1, &empty_store)
        );
        assert_eq!(
            interpret(&prog, &input("4").unwrap(), &progs).unwrap().0,
            eval(&expression("4").unwrap().1, &empty_store)
        );
        assert_eq!(
            interpret(&prog, &input("0").unwrap(), &progs).unwrap().0,
            eval(&expression("1000").unwrap().1, &empty_store)
        );
        assert_eq!(
            interpret(&prog, &input("2").unwrap(), &progs).unwrap().0,
            eval(&expression("137").unwrap().1, &empty_store)
        );
    }
}
