use clap::Parser;
use indexmap::IndexMap;
use whilers::{
    interpret::input,
    lang::{Prog, ProgName},
    output::{generate_output, OutputFormat},
};

#[derive(Parser, Debug)]
struct Args {
    input_prog: String,
    input_expr: String,
}

fn main() {
    let args = Args::parse();
    let dir = std::fs::read_dir("./").unwrap();
    let mut progs: IndexMap<ProgName, Prog> = Default::default();
    for file in dir {
        let Ok(file) = file else {
            continue;
        };

        if !file.file_name().to_string_lossy().ends_with(".while") {
            continue;
        }

        let src = std::fs::read_to_string(file.path()).unwrap();

        let prog = whilers::parser::parse(&src).unwrap();

        progs.insert(prog.prog_name.clone(), prog);
    }

    let prog = progs
        .get(&ProgName(args.input_prog))
        .expect("input program name should ");

    let output = generate_output(
        prog,
        &input(&args.input_expr, &progs).unwrap(),
        &progs,
        &OutputFormat::NestedListOfAtoms,
        false,
    );

    match output {
        whilers::output::Output::Text(t) => {
            println!("{t}");
        }
        whilers::output::Output::Error(e) => {
            println!("{e}");
            panic!()
        }
        whilers::output::Output::None => {
            panic!("Output should never be None?");
        }
    }
}
