#![warn(clippy::all, rust_2018_idioms)]

mod app;
pub use app::App;
pub mod atoms;
pub mod editor;
pub mod extended_to_core;
pub mod highlight;
pub mod interpret;
pub mod output;
pub mod parser;
pub mod utils;
