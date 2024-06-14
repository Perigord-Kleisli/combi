{
  inputs = {
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "nixpkgs/nixos-unstable";
  };

  outputs = {
    fenix,
    nixpkgs,
    ...
  }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      overlays = [fenix.overlays.default];
    };
  in {
    devShells.${system}.default = pkgs.mkShell {
      packages = with pkgs; [
        (fenix.packages.${system}.complete.withComponents [
          "cargo"
          "clippy"
          "rust-src"
          "rustc"
          "rustfmt"
        ])
        bacon
        cargo-watch
        lldb
        rust-analyzer-nightly
      ];
    };
  };
}
