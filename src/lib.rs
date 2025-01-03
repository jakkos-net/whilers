#![warn(clippy::all, rust_2018_idioms)]

mod app;
mod niltree;
mod prog_as_data;
mod variables;
pub use app::App;
pub mod atoms;
pub mod editor;
pub mod extended_to_core;
pub mod highlight;
pub mod interpret;
pub mod lang;
pub mod output;
pub mod parser;
pub mod utils;
#[cfg(target_arch = "wasm32")]
pub mod web;
