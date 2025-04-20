use egui::{
    CentralPanel, Color32, ComboBox, Context, FontId, RichText, ScrollArea, Style, TextEdit,
    TextStyle, Ui, Vec2, Visuals, Window,
};

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

use crate::{
    highlight::layouter,
    interpret::input,
    lang::{Prog, ProgName},
    output::{generate_output, Output, OutputFormat},
    parser::{get_prog_name_string_fast, parse},
};

#[derive(Serialize, Deserialize)]
pub struct EditorState {
    tabs: Vec<Tab>,
    active_tab_id: usize,
    input: String,
    output: Output,
    output_format: OutputFormat,
    debug: bool,
    show_ui_settings: bool,
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
            show_ui_settings: false,
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
                run_ui(ui, state);
                ui.add_space(spacing);
                convert_ui(ui, state);
                ui.add_space(spacing);
                output_ui(ui, state);

                ui.add_space(spacing);
                ui.horizontal(|ui| {
                    if ui.small_button("Reset application").clicked() {
                        *state = EditorState::default();
                        ctx.memory_mut(|m| *m = Default::default());
                        ctx.set_style(style());
                    }
                    if ui.small_button("Ui settings").clicked() {
                        state.show_ui_settings = true;
                    }
                });

                ui.add_space(spacing);

                build_info(ui);

                Window::new("ui settings")
                    .open(&mut state.show_ui_settings)
                    .show(ctx, |ui| ctx.settings_ui(ui))
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
        ui.add_space(10.0);
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
                    // if we make create a new tab, it gets added to the end, set it active
                    state.active_tab_id = state.tabs.len().saturating_sub(1);
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
                    #[cfg(target_arch = "wasm32")]
                    if ui.small_button("💾").clicked() {
                        crate::web::save_file(&tab.code, &format!("{}.while", &tab.title));
                    }
                    if ui.button("X").clicked() {
                        to_remove = Some(id);
                    }
                    ui.label("|");
                }

                if let Some(id_to_remove) = to_remove {
                    state.tabs.remove(id_to_remove);
                    // if we delete a tab that occurs before our active tab, our active tab is now 1 idx earlier.
                    if id_to_remove < state.active_tab_id {
                        state.active_tab_id = state.active_tab_id.saturating_sub(1);
                    }
                }
                if ui.button("+").clicked() {
                    state.tabs.push(Default::default());
                    // if we make create a new tab, it becomes the last tab, set it active
                    state.active_tab_id = state.tabs.len().saturating_sub(1);
                }
            });
        });

    if state.tabs.is_empty() {
        ui.label("No open files, try clicking '+' or importing a file");
        return;
    }
    ui.separator();
    if let Some(tab) = state.tabs.get_mut(state.active_tab_id) {
        code_ui(ui, &mut tab.code);
        tab.update_title();
    } else {
        ui.label("Selected source code file doesn't exist!");
    }
    ui.separator();
}

fn code_ui(ui: &mut Ui, src: &mut String) {
    let min_lines = 10;
    let num_lines = src.split("\n").count().max(min_lines);
    let max_num_chars = num_lines.to_string().len();
    let font_id = ui
        .style()
        .text_styles
        .get(&TextStyle::Monospace)
        .cloned()
        .unwrap_or_default();
    let max_num_width = ui
        .painter()
        .layout(
            format!("{num_lines}"),
            font_id.clone(),
            Default::default(),
            f32::MAX,
        )
        .rect
        .width();
    let mut line_numbers_str = String::with_capacity(num_lines * max_num_chars);
    for i in 1..=num_lines {
        let num_str = i.to_string();
        // add padding to right justify
        for _ in 0..(max_num_chars - num_str.len()) {
            line_numbers_str.push_str(" ");
        }
        line_numbers_str.push_str(&format!("{i}"));
        if i < num_lines {
            line_numbers_str.push_str("\n");
        }
    }
    let margin = Vec2::new(4.0, 2.0);
    let line_numbers = TextEdit::multiline(&mut line_numbers_str)
        .min_size(Vec2::new(max_num_width, 0.))
        .margin(Vec2::new(0., margin.y))
        .desired_width(max_num_width)
        .font(font_id.clone())
        .desired_rows(min_lines)
        .horizontal_align(egui::Align::Max)
        .frame(false)
        .interactive(false);
    ui.horizontal(|ui| {
        ui.add(line_numbers);
        ui.add(
            TextEdit::multiline(src)
                .font(font_id.clone())
                .lock_focus(true)
                .margin(margin)
                .desired_rows(min_lines)
                .desired_width(f32::INFINITY)
                .layouter(&mut layouter()),
        );
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
        state.output = match input(&state.input, &all_progs) {
            Ok(input) => generate_output(&prog, &input, &all_progs, &output_format, state.debug),
            Err(e) => Output::Error(e.to_string()),
        }
    }
}

fn output_ui(ui: &mut Ui, state: &mut EditorState) {
    ui.heading("Output");
    ui.separator();
    let output = &state.output;
    match output {
        Output::Text(str) | Output::Error(str) => {
            // todo_minor remove temp
            let mut temp = str.to_string();
            ui.add(
                TextEdit::multiline(&mut temp)
                    .font(TextStyle::Monospace)
                    .code_editor()
                    .desired_rows(5)
                    .desired_width(f32::INFINITY)
                    .layouter(&mut layouter()),
            );
        }
        Output::None => {
            ui.label("No output. Click run to generate an output!");
        }
    }
    ui.separator();
}

pub fn style() -> Style {
    Style {
        visuals: Visuals::light(),
        ..Default::default()
    }
}

pub fn build_info(ui: &mut Ui) {
    let s = format!(
        "{}.{}.{}",
        env!("VERGEN_GIT_COMMIT_DATE"),
        env!("VERGEN_GIT_BRANCH"),
        env!("VERGEN_GIT_SHA"),
    );
    ui.label(RichText::new(s).weak());
}
