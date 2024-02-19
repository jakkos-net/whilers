use egui::{
    CentralPanel, Color32, ComboBox, Context, RichText, ScrollArea, TextEdit, TextStyle, Ui,
};

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

use crate::{
    highlight::layouter,
    interpret::input,
    output::{generate_output, Output, OutputFormat},
    parser::{get_prog_name_string_fast, parse, Prog, ProgName},
};

#[derive(Serialize, Deserialize)]
pub struct EditorState {
    tabs: Vec<Tab>,
    active_tab_id: usize,
    input: String,
    output: Output,
    output_format: OutputFormat,
    debug: bool,
}

#[derive(Serialize, Deserialize)]
struct Tab {
    title: String,
    code: String,
}

impl Default for Tab {
    fn default() -> Self {
        Tab::from_code(include_str!("../programs/prog.while"))
    }
}

impl Tab {
    fn from_code(code: &str) -> Self {
        let code = code.to_string();
        Tab {
            title: get_prog_name_string_fast(&code),
            code,
        }
    }

    fn update_title(&mut self) {
        self.title = get_prog_name_string_fast(&self.code)
    }
}

impl Default for EditorState {
    fn default() -> Self {
        Self {
            tabs: vec![Default::default()],
            active_tab_id: 0,
            input: "3".into(),
            output: Default::default(),
            output_format: OutputFormat::NilTree,
            debug: false,
        }
    }
}

pub fn ui(ctx: &Context, state: &mut EditorState) {
    CentralPanel::default().show(ctx, |ui| {
        ScrollArea::vertical()
            .id_source("top level scroll")
            .show(ui, |ui| {
                let spacing = 15.0;
                title_ui(ui);
                code_tabs_ui(ctx, ui, state);
                ui.separator();
                run_ui(ui, state);
                ui.add_space(spacing);
                convert_ui(ui, state);
                ui.add_space(spacing);
                output_ui(ui, state);

                if ui.small_button("Reset").clicked() {
                    *state = EditorState::default();
                    ctx.memory_mut(|m| *m = Default::default());
                }
            });
    });
}

fn title_ui(ui: &mut Ui) {
    ui.label(RichText::new("whilers").strong().size(30.0));
    ui.separator();
    ui.horizontal(|ui| {
        ui.label(
            "An editor and interpreter for the While language from Prof. Bernhard Reus' textbook:",
        );
        ui.hyperlink_to(
            "Limits of Computation - From a Programming Perspective",
            "https://limits.bernhardreus.com/",
        );
    });
    ui.horizontal(|ui| {
        ui.label("Source code:");
        ui.hyperlink_to(
            "github.com/jakkos-net/whilers",
            "https://www.github.com/jakkos-net/whilers",
        );
    });
    ui.horizontal(|ui| {
        ui.label("Syntax highlighting rules:");
        ui.hyperlink_to(
            "github.com/tobydennison/WHILE-Syntax-Highlighter",
            "https://github.com/tobydennison/WHILE-Syntax-Highlighter",
        );
    });
    ui.separator();
}

fn import_files_ui(ctx: &Context, ui: &mut Ui, state: &mut EditorState) {
    ui.label("Drag and drop files to import!");

    ctx.input(|i| {
        i.raw.dropped_files.iter().for_each(|file| {
            if let Some(bytes) = &file.bytes {
                if let Ok(s) = String::from_utf8(bytes.to_vec()) {
                    state.tabs.push(Tab::from_code(&s));
                    state.active_tab_id += 1;
                }
            }
        })
    });
}

fn code_tabs_ui(ctx: &Context, ui: &mut Ui, state: &mut EditorState) {
    ui.heading("Code");

    import_files_ui(ctx, ui, state);
    ScrollArea::horizontal()
        .id_source("code tabs")
        .show(ui, |ui| {
            ui.horizontal(|ui| {
                ui.label("|");
                let mut to_remove = None;
                for (id, tab) in state.tabs.iter().enumerate() {
                    let name = format!("{} - {}", id.to_string(), tab.title);
                    if id == state.active_tab_id {
                        ui.label(
                            RichText::new(name)
                                .strong()
                                .underline()
                                .color(Color32::BLACK)
                                .background_color(Color32::from_gray(200)),
                        );
                    } else {
                        if ui.button(name).clicked() {
                            state.active_tab_id = id;
                        }
                    }
                    if ui.button("X").clicked() {
                        to_remove = Some(id);
                    }
                    ui.label("|");
                }

                if let Some(id_to_remove) = to_remove {
                    state.tabs.remove(id_to_remove);
                    if state.active_tab_id > 0 {
                        state.active_tab_id -= 1;
                    }
                }
                if ui.button("+").clicked() {
                    state.tabs.push(Default::default());
                    state.active_tab_id += 1;
                }
            });
        });

    if state.tabs.is_empty() {
        ui.label("No open files, try clicking '+' or importing a file");
        return;
    }

    ScrollArea::both().id_source("code").show(ui, |ui| {
        if let Some(tab) = state.tabs.get_mut(state.active_tab_id) {
            ui.add(
                TextEdit::multiline(&mut tab.code)
                    .font(TextStyle::Monospace)
                    .code_editor()
                    .desired_rows(20)
                    .lock_focus(true)
                    .desired_width(f32::INFINITY)
                    .layouter(&mut layouter()),
            );

            tab.update_title();
        } else {
            ui.label("Selected source code file doesn't exist!");
        }
    });
}

fn run_ui(ui: &mut Ui, state: &mut EditorState) {
    ui.heading("Run");
    ui.horizontal(|ui| {
        ui.label("Input:");
        ui.add(
            TextEdit::multiline(&mut state.input)
                .font(TextStyle::Monospace)
                .code_editor()
                .desired_rows(1)
                .lock_focus(true)
                .desired_width(f32::INFINITY)
                .layouter(&mut layouter()),
        );
    });
    ui.horizontal(|ui| {
        let old_output_format = state.output_format;
        ui.label("Output format:");
        ComboBox::from_id_source("Output format")
            .selected_text(format!("{}", state.output_format))
            .show_ui(ui, |ui| {
                ui.style_mut().wrap = Some(false);
                ui.set_min_width(60.0);

                use OutputFormat::*;
                for fmt in [
                    NilTree,
                    Integer,
                    ListOfIntegers,
                    NestedListOfIntegers,
                    NestedListOfAtoms,
                ] {
                    ui.selectable_value(&mut state.output_format, fmt, fmt.to_string());
                }
            });
        let new_output_format = state.output_format;
        if new_output_format != old_output_format {
            state.output = Output::None;
        };

        ui.label("Debug?:");
        ui.checkbox(&mut state.debug, "");
    });
    if ui.button("Run").clicked() {
        run(state, state.output_format)
    }
}

fn convert_ui(ui: &mut Ui, state: &mut EditorState) {
    ui.heading("Convert");
    ui.horizontal(|ui| {
        let mut output_format = None;
        if ui.button("To Core While").clicked() {
            output_format = Some(OutputFormat::CoreWhile)
        }

        if ui.button("To programs as data").clicked() {
            output_format = Some(OutputFormat::ProgramAsData)
        }

        if let Some(output_format) = output_format {
            run(state, output_format);
        }
    });
}

fn run(state: &mut EditorState, output_format: OutputFormat) {
    let mut all_progs: IndexMap<ProgName, Prog> = Default::default();

    // check that all open files contain valid code and add them to a map of parsed programs
    for (id, tab) in state.tabs.iter().enumerate() {
        match parse(&tab.code) {
            Ok(prog) => {
                if let Some(prog) = all_progs.insert(prog.prog_name.clone(), prog) {
                    state.output = Output::Error(format!(
                                    "Multiple programs have the same name: '{}'\nFirst duplicate found in tab {id}",
                                    prog.prog_name
                                ));
                    return;
                }
            }
            Err(e) => {
                state.output = Output::Error(format!("Failed to parse src file in tab {id}!\n{e}"));
                return;
            }
        }
    }

    // try and get the current tab's program and run it
    if let Some((_, prog)) = all_progs.get_index(state.active_tab_id) {
        state.output = match input(&state.input) {
            Ok(input) => generate_output(&prog, &input, &all_progs, &output_format, state.debug),
            Err(e) => Output::Error(e.to_string()),
        }
    }
}

fn output_ui(ui: &mut Ui, state: &mut EditorState) {
    ui.heading("Output");
    let output = &state.output;
    ScrollArea::both().id_source("output").show(ui, |ui| {
        match output {
            Output::Text(str) | Output::Error(str) => {
                // todo_minor remove temp
                let mut temp = str.to_string();
                ui.add(
                    TextEdit::multiline(&mut temp)
                        .font(TextStyle::Monospace)
                        .code_editor()
                        .desired_rows(10)
                        .desired_width(f32::INFINITY)
                        .layouter(&mut layouter()),
                );
            }
            Output::None => {
                ui.label("No output. Click run to generate an output!");
            }
        }
    });
}
