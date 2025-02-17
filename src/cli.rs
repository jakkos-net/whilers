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
    let dir = std::fs::read_dir("./").expect("Current directory should be accessible");
    let mut progs: IndexMap<ProgName, Prog> = Default::default();
    for file in dir {
        let Ok(file) = file else {
            continue;
        };

        if !file.file_name().to_string_lossy().ends_with(".while") {
            continue;
        }

        let src = std::fs::read_to_string(file.path())
            .expect(".while files in the current directory should be readable");

        let prog = whilers::parser::parse(&src)
            .expect(".while files in the current directory should be valid");

        progs.insert(prog.prog_name.clone(), prog);
    }

    let prog = progs
        .get(&ProgName(args.input_prog))
        .expect("Specified program should exist in the current directory");

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
        }
        whilers::output::Output::None => {
            panic!("Output should never be None, please report this as a bug!");
        }
    }
}
