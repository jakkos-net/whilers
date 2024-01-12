// This module contains functionality for syntax highlighting

use std::sync::Arc;

use egui::{
    text::{LayoutJob, LayoutSection},
    Color32, FontId, Galley, TextFormat, Ui,
};
use once_cell::sync::Lazy;
use syntect::{
    easy::HighlightLines,
    highlighting::ThemeSet,
    parsing::{SyntaxDefinition, SyntaxSet, SyntaxSetBuilder},
    util::LinesWithEndings,
};

static SYNTAX_THEMES: Lazy<ThemeSet> = Lazy::new(ThemeSet::load_defaults);
static SYNTAX_SET: Lazy<SyntaxSet> = Lazy::new(|| {
    let mut builder = SyntaxSetBuilder::new();
    builder.add(
        SyntaxDefinition::load_from_str(include_str!("./while.sublime-syntax"), true, None)
            .unwrap(),
    );
    builder.build()
});

pub fn layouter() -> impl FnMut(&Ui, &str, f32) -> Arc<Galley> {
    |ui: &Ui, str: &str, wrap_width: f32| {
        let mut layout_job = layout_job(str);
        layout_job.wrap.max_width = wrap_width;
        ui.fonts(|f| f.layout_job(layout_job))
    }
}

fn layout_job(str: &str) -> LayoutJob {
    try_layout_job(str).unwrap_or_else(|_| {
        LayoutJob::simple(
            str.into(),
            FontId::monospace(12.0),
            Color32::LIGHT_GRAY,
            f32::INFINITY,
        )
    })
}

fn try_layout_job(str: &str) -> anyhow::Result<LayoutJob> {
    let mut job = LayoutJob {
        text: str.into(),
        ..Default::default()
    };
    let mut highlighter = HighlightLines::new(
        SYNTAX_SET.find_syntax_by_extension("while").unwrap(),
        &SYNTAX_THEMES.themes["base16-ocean.dark"],
    );
    for line_str in LinesWithEndings::from(str) {
        for (style, range) in highlighter.highlight_line(line_str, &SYNTAX_SET)? {
            let fg = style.foreground;
            let text_color = Color32::from_rgb(fg.r, fg.g, fg.b);
            job.sections.push(LayoutSection {
                leading_space: 0.0,
                byte_range: as_byte_range(str, range),
                format: TextFormat {
                    font_id: FontId::monospace(12.0),
                    color: text_color,
                    ..Default::default()
                },
            });
        }
    }

    Ok(job)
}

fn as_byte_range(whole: &str, range: &str) -> std::ops::Range<usize> {
    let whole_start = whole.as_ptr() as usize;
    let range_start = range.as_ptr() as usize;
    assert!(whole_start <= range_start);
    assert!(range_start + range.len() <= whole_start + whole.len());
    let offset = range_start - whole_start;
    offset..(offset + range.len())
}
