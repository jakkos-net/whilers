use anyhow::bail;
use indexmap::IndexMap;

use crate::{
    extended_to_core::prog_to_core,
    lang::{Block, Expression, Prog, ProgName, Statement},
    utils::indent,
    variables::Variables,
};

pub fn unparse_prog(prog: &Prog, progs: &IndexMap<ProgName, Prog>) -> anyhow::Result<String> {
    let core_prog = prog_to_core(prog, progs)?;
    unparse_core_prog(&core_prog)
}

pub fn unparse_core_prog(prog: &Prog) -> anyhow::Result<String> {
    let vars = Variables::new(prog);
    Ok(format!(
        "[{}, {}, {}]",
        vars.get(&prog.input_var),
        unparse_block(&prog.body, &vars)?,
        vars.get(&prog.output_var)
    ))
}

pub fn unparse_block(block: &Block, vars: &Variables) -> anyhow::Result<String> {
    if block.0.is_empty() {
        return Ok("[]".into());
    }
    Ok(format!(
        "[\n{}\n]",
        block
            .0
            .iter()
            .map(|stmt| unparse_stmt(stmt, vars).map(|s| indent(&s)))
            .collect::<Result<Vec<_>, _>>()?
            .join(",\n")
    ))
}

pub fn unparse_stmt(stmt: &Statement, vars: &Variables) -> anyhow::Result<String> {
    Ok(format!(
        "[{}]",
        match stmt {
            Statement::Assign(var, expr) =>
                format!("@:=, {}, {}", vars.get(var), unparse_expr(expr, vars)?),
            Statement::While { cond, body } => format!(
                "@while, {}, {}",
                unparse_expr(cond, vars)?,
                unparse_block(body, vars)?
            ),
            Statement::If { cond, then, or } => format!(
                "@if, {}, {}, {}",
                unparse_expr(cond, vars)?,
                unparse_block(then, vars)?,
                unparse_block(or, vars)?
            ),
            _ => bail!("Cannot unparse extended While statment: {stmt}"),
        }
    ))
}

pub fn unparse_expr(expr: &Expression, vars: &Variables) -> anyhow::Result<String> {
    Ok(format!(
        "[{}]",
        match expr {
            Expression::Cons(e1, e2) => format!(
                "@cons, {}, {}",
                unparse_expr(e1, vars)?,
                unparse_expr(e2, vars)?
            ),
            Expression::Hd(e) => format!("@hd, {}", unparse_expr(e, vars)?),
            Expression::Tl(e) => format!("@tl, {}", unparse_expr(e, vars)?),
            Expression::Nil => "@quote, nil".into(),
            Expression::Var(var) => format!("@var, {}", vars.get(var)),
            _ => bail!("Cannot unparse extended While expression: {expr} "),
        }
    ))
}

#[cfg(test)]
mod tests {
    use super::unparse_prog;
    use crate::parser::parse;

    #[test]
    fn test_unparse_simple_eq() {
        let s = include_str!("../programs/simple_eq.while");
        let prog = parse(s).unwrap();
        let progs = Default::default();
        // just make sure it doesn't error out
        unparse_prog(&prog, &progs).unwrap();
    }
}
