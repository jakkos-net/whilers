use crate::{
    lang::{Block, Expression, Prog, Statement},
    utils::indent,
    variables::Variables,
};

pub fn unparse_prog(prog: &Prog) -> String {
    let vars = Variables::new(prog);
    format!(
        "[{}, {}, {}]",
        vars.get(&prog.input_var),
        unparse_block(&prog.body, &vars),
        vars.get(&prog.output_var)
    )
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
