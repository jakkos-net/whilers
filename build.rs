fn main() {
    let git_info = vergen_gitcl::GitclBuilder::all_git().unwrap();
    vergen_gitcl::Emitter::default()
        .fail_on_error()
        .add_instructions(&git_info)
        .unwrap()
        .emit()
        .unwrap();
}
