# whilers

An online editor and interpreter for the While language from Prof. Bernhard Reus’ textbook [Limits of Computation - From a Programming Perspective](https://limits.bernhardreus.com/).

[Click here to use the live web version!](https://jakkos.net/whilers)

## gui
- [Install Rust](https://www.rust-lang.org/learn/get-started)
- Clone the repo
- `cargo run --release`

## cli
- [Install Rust](https://www.rust-lang.org/learn/get-started)
- Clone the repo
- `cargo build --release --bin whilers_cli`
- An executable `whilers_cli` will be created in the `target/release` directory

You can then run programs with `./whilers_cli myprog.while nil`. Only "nested atoms" output is currently supported.

### web gui
- [Install Rust](https://www.rust-lang.org/learn/get-started)
- Install trunk: `cargo install trunk --locked`
- Clone the repo
- Run `trunk build --release`
- The dist folder will contain the web files to host

GitHub actions should build a .zip containing the web files whenever changes are pushed to the master branch. Click [here](https://github.com/jakkos-net/whilers/actions/workflows/pages.yml), open the most recent (top) workflow run and scroll to the bottom (you might need to be logged in).





