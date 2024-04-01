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
        (self: super: {rustToolchain = super.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;})
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
            rustToolchain
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
