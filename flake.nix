{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay}:
    let overlays = [
        (import rust-overlay)
      ];
    in
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit overlays system; };
        lib = pkgs.lib;
      in 
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            (rust-bin.stable."1.74.0".default.override {
              extensions = [ "rust-src" "rust-analyzer" ];
              targets = [ "wasm32-unknown-unknown" ];
            })
            just
            bacon
            gcc
            openssl
            pkg-config
            libGL
            cmake
            glib
            fontconfig
            atk
            gtk3
            trunk
          ];

          LD_LIBRARY_PATH = with pkgs; lib.makeLibraryPath [
            libGL
            libxkbcommon
            wayland
            xorg.libX11
            xorg.libXcursor
            xorg.libXi
            xorg.libXrandr
          ];
        };
      });
}
