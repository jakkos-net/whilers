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

        let Ok(src) = std::fs::read_to_string(file.path()) else {
            continue;
        };

        let Ok(prog) = whilers::parser::parse(&src) else {
            continue;
        };

        progs.insert(prog.prog_name.clone(), prog);
    }

    // we have to print and return rather than panic-ing because the automarker
    let prog_src = match std::fs::read_to_string(args.input_prog) {
        Ok(src) => src,
        Err(e) => {
            println!("{e}");
            return;
        }
    };
    let prog = match whilers::parser::parse(&prog_src) {
        Ok(prog) => prog,
        Err(e) => {
            println!("{e}");
            return;
        }
    };

    let output = generate_output(
        &prog,
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
            println!("Output should never be None, please report this as a bug!");
        }
    }
}
