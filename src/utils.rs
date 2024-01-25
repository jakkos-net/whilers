pub fn indent(s: &str) -> String {
    format!("\t{}", s.replace("\n", "\n\t"))
}
