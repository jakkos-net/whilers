// This module contains functions for executing While programs

use std::collections::HashMap;

use anyhow::{bail, Context};
use indexmap::IndexMap;
use regex::Regex;

use crate::{
    extended_to_core::{list_to_cons, prog_to_core, switch_to_ifs},
    lang::{Block, Expression, Prog, ProgName, Statement},
    niltree::{cons, NilTree},
    parser::expression,
    prog_as_data::unparse_prog,
    variables::VarName,
};

const MAX_EXEC_STEPS: u32 = 10_000_000;

pub fn interpret(
    main_prog: &Prog,
    input: &NilTree,
    progs: &IndexMap<ProgName, Prog>,
) -> anyhow::Result<(NilTree, ExecState)> {
    let mut state = ExecState::new(&main_prog.prog_name);
    match interpret_with_state(main_prog, input, progs, &mut state) {
        Ok(_) => Ok((state.get(&main_prog.output_var).clone(), state)),
        Err(err) => Err(err),
    }
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
    // state.step()?;
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
            while eval(cond, state).as_bool() {
                exec_block(body, state, progs)?
            }
        }
        Statement::If { cond, then, or } => {
            let cond = eval(cond, state);
            let block = if cond.as_bool() { then } else { or };
            exec_block(block, state, progs)?
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
    use Expression as E;
    match expr {
        E::Cons(e1, e2) => cons(&eval(e1, store), &eval(e2, store)),
        E::Hd(e) => eval(e, store).hd(),
        E::Tl(e) => eval(e, store).tl(),
        E::Nil => NilTree::Nil,
        E::Var(var) => store.get(var).clone(),
        E::Num(n) => NilTree::Num(*n),
        E::Bool(b) => match b {
            true => NilTree::Num(1),
            false => NilTree::Nil,
        },
        E::List(v) => eval(&list_to_cons(&v[..]), store),
        E::Eq(a, b) => match eval(a, store) == eval(b, store) {
            true => NilTree::Num(1),
            false => NilTree::Nil,
        },
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

    // Ideally this would return a reference but it doens't like it due to NilTree having a drop impl
    pub fn get(&self, var: &VarName) -> NilTree {
        let (_prog_name, store) = self
            .macro_stack
            .last()
            .expect("Macro stack should never be empty!");
        store.get(var).unwrap_or(&NilTree::Nil).clone()
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

pub fn input(s: &str, progs: &IndexMap<ProgName, Prog>) -> anyhow::Result<NilTree> {
    let s = replace_progs_as_data(s, progs)?;
    match expression(&s) {
        Ok((_, expr)) => Ok(eval(&expr, &ExecState::new(&ProgName("input".into())))),
        Err(e) => bail!("Failed to parse input:\n{:?}", e),
    }
}

// replace `prog` with the programs as data representation of prog
fn replace_progs_as_data(s: &str, progs: &IndexMap<ProgName, Prog>) -> anyhow::Result<String> {
    let prog_as_data_regex = Regex::new("`([A-Za-z0-9][_A-Za-z0-9]*)`").unwrap();
    let mut s = s.to_string();
    while let Some(captures) = prog_as_data_regex.captures(&s) {
        let backticked_prog_name = captures.get(0).unwrap().as_str();
        let prog_name = ProgName(captures.get(1).unwrap().as_str().to_string());
        let prog = progs
            .get(&prog_name)
            .with_context(|| "`{prog_name}` in input, but that program does not exist!")?;
        let core_prog = prog_to_core(&prog, progs)?;
        let prog_as_data = unparse_prog(&core_prog);

        s = s.replace(backticked_prog_name, &prog_as_data);
    }
    Ok(s)
}
#[cfg(test)]
mod tests {

    use crate::{
        interpret::{eval, input, ExecState},
        lang::ProgName,
        parser::{expression, parse},
    };

    use super::interpret;

    #[test]
    pub fn test_add() {
        let prog = parse(include_str!("../programs/add.while")).unwrap();
        let progs = Default::default();

        let input = input("[3,4]", &progs).unwrap();

        let progs = Default::default();

        let output = interpret(&prog, &input, &progs);

        let empty_store = ExecState::new(&ProgName("testing".into()));

        assert_eq!(
            output.unwrap().0,
            eval(&expression("7").unwrap().1, &empty_store)
        );
    }

    #[test]
    pub fn test_switch1() {
        let prog = parse(include_str!("../programs/switch1.while")).unwrap();

        let progs = Default::default();

        let empty_store = ExecState::new(&ProgName("testing".into()));

        assert_eq!(
            interpret(&prog, &input("3", &progs).unwrap(), &progs)
                .unwrap()
                .0,
            eval(&expression("3").unwrap().1, &empty_store)
        );
        assert_eq!(
            interpret(&prog, &input("4", &progs).unwrap(), &progs)
                .unwrap()
                .0,
            eval(&expression("4").unwrap().1, &empty_store)
        );
        assert_eq!(
            interpret(&prog, &input("0", &progs).unwrap(), &progs)
                .unwrap()
                .0,
            eval(&expression("1000").unwrap().1, &empty_store)
        );
    }

    #[test]
    pub fn test_switch2() {
        let prog = parse(include_str!("../programs/switch2.while")).unwrap();

        let progs = Default::default();

        let empty_store = ExecState::new(&ProgName("testing".into()));

        assert_eq!(
            interpret(&prog, &input("3", &progs).unwrap(), &progs)
                .unwrap()
                .0,
            eval(&expression("3").unwrap().1, &empty_store)
        );
        assert_eq!(
            interpret(&prog, &input("4", &progs).unwrap(), &progs)
                .unwrap()
                .0,
            eval(&expression("4").unwrap().1, &empty_store)
        );
        assert_eq!(
            interpret(&prog, &input("0", &progs).unwrap(), &progs)
                .unwrap()
                .0,
            eval(&expression("1000").unwrap().1, &empty_store)
        );
        assert_eq!(
            interpret(&prog, &input("2", &progs).unwrap(), &progs)
                .unwrap()
                .0,
            eval(&expression("137").unwrap().1, &empty_store)
        );
    }
}
