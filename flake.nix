{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };
  outputs = {nixpkgs, rust-overlay, ...}: let
    systems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];
    overlays = [ (import rust-overlay) ];
    forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f (import nixpkgs {inherit system overlays;}));
  in 
  {
    devShells = forAllSystems (pkgs: {
      default = pkgs.mkShell {
        buildInputs = with pkgs; [
          (rust-bin.fromRustupToolchainFile ./rust-toolchain.toml)
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
  };
}
