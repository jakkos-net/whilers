// This module contains functions for executing While programs

use std::collections::HashMap;

use anyhow::{bail, Context};
use indexmap::IndexMap;

use crate::{
    extended_to_core::{list_to_core, num_to_core, switch_to_ifs},
    parser::{expression, Block, Expression, NilTree, Prog, ProgName, Statement, VarName},
};

const MAX_EXEC_STEPS: u32 = 1_000_000;

pub fn interpret(
    main_prog: &Prog,
    input: &NilTree,
    progs: &IndexMap<ProgName, Prog>,
) -> anyhow::Result<(NilTree, ExecState)> {
    let mut state = ExecState::new(&main_prog.prog_name);
    interpret_with_state(main_prog, input, progs, &mut state)?;
    Ok((state.get(&main_prog.output_var).clone(), state))
}

fn interpret_with_state(
    main_prog: &Prog,
    input: &NilTree,
    progs: &IndexMap<ProgName, Prog>,
    state: &mut ExecState,
) -> anyhow::Result<()> {
    state.set(&main_prog.input_var, input);
    exec_block(&main_prog.body, state, progs)?;
    Ok(())
}

fn exec_block(
    block: &Block,
    state: &mut ExecState,
    progs: &IndexMap<ProgName, Prog>,
) -> anyhow::Result<()> {
    state.step()?;
    for stmt in &block.0 {
        exec(stmt, state, progs)?
    }
    Ok(())
}

fn exec(
    stmt: &Statement,
    state: &mut ExecState,
    progs: &IndexMap<ProgName, Prog>,
) -> anyhow::Result<()> {
    state.step()?;
    match stmt {
        Statement::Assign(var, expr) => state.set(var, &eval(expr, state)),
        Statement::While { cond, body } => {
            while eval(cond, state) != NilTree::Nil {
                exec_block(body, state, progs)?
            }
        }
        Statement::If { cond, then, or } => {
            let cond = eval(cond, state);
            match cond {
                NilTree::Nil => exec_block(or, state, progs)?,
                _ => exec_block(then, state, progs)?,
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
            let input_val = eval(&input_expr, state);
            state.push_macro(prog_name)?;

            interpret_with_state(&macro_prog, &input_val, progs, state)?;

            let output_value = state.get(&macro_prog.output_var).clone();

            state.pop_macro()?;

            state.set(var, &output_value);
        }
        Statement::Switch {
            cond,
            cases,
            default,
        } => exec(&switch_to_ifs(cond, cases, default), state, progs)?,
    }

    Ok(())
}

fn eval(expr: &Expression, store: &ExecState) -> NilTree {
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

pub struct ExecState {
    macro_stack: IndexMap<ProgName, HashMap<VarName, NilTree>>,
    change_history: Vec<(ProgName, VarName, NilTree)>,
    steps: u32,
    max_steps: u32,
}

impl ExecState {
    pub fn new(main_prog_name: &ProgName) -> ExecState {
        ExecState {
            macro_stack: IndexMap::from([(main_prog_name.clone(), Default::default())]),
            change_history: Default::default(),
            steps: 0,
            max_steps: MAX_EXEC_STEPS,
        }
    }

    pub fn set(&mut self, var: &VarName, data: &NilTree) {
        let (prog_name, store) = self
            .macro_stack
            .last_mut()
            .expect("Macro stack should never be empty!");
        store.insert(var.clone(), data.clone());
        self.change_history
            .push((prog_name.clone(), var.clone(), data.clone()));
    }

    pub fn get(&self, var: &VarName) -> &NilTree {
        let (_prog_name, store) = self
            .macro_stack
            .last()
            .expect("Macro stack should never be empty!");
        store.get(var).unwrap_or(&NilTree::Nil)
    }

    pub fn get_history(&self) -> &Vec<(ProgName, VarName, NilTree)> {
        &self.change_history
    }

    pub fn push_macro(&mut self, prog_name: &ProgName) -> anyhow::Result<()> {
        if self.macro_stack.contains_key(prog_name) {
            bail!("Tried to call macro '{prog_name}', however we are already inside a call to '{prog_name}', recursive macro calls are not allowed!");
        }

        self.macro_stack
            .insert(prog_name.clone(), Default::default());

        Ok(())
    }

    pub fn pop_macro(&mut self) -> anyhow::Result<()> {
        if self.macro_stack.len() <= 1 {
            bail!("Cannot pop macro, no macros exist to pop!");
        }
        self.macro_stack.pop();
        Ok(())
    }

    pub fn step(&mut self) -> anyhow::Result<()> {
        self.steps += 1;
        if self.steps > self.max_steps {
            bail!("Maximum execution step count exceeded, program terminated!");
        }
        Ok(())
    }
}

pub fn input(s: &str) -> anyhow::Result<NilTree> {
    match expression(s) {
        Ok((_, expr)) => Ok(eval(&expr, &ExecState::new(&ProgName("input".into())))),
        Err(e) => bail!("Failed to parse input:\n{:?}", e),
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        interpret::{eval, input, ExecState},
        parser::{expression, parse, ProgName},
    };

    use super::interpret;

    #[test]
    pub fn test_add() {
        let prog = parse(include_str!("../programs/add.while")).unwrap();

        let input = input("[3,4]").unwrap();

        let progs = Default::default();

        let output = interpret(&prog, &input, &progs);

        let empty_store = ExecState::new(&ProgName("testing".into()));

        assert_eq!(
            output.unwrap().0,
            eval(&expression("7").unwrap().1, &empty_store)
        );
    }

    #[test]
    pub fn test_switch() {
        let prog = parse(include_str!("../programs/swTest.while")).unwrap();

        let progs = Default::default();

        let empty_store = ExecState::new(&ProgName("testing".into()));

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
