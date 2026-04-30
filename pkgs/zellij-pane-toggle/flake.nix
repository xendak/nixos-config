{
  description = "zellij-pane-toggle — background WASM plugin for atomic pane toggling";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      rust-overlay,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ rust-overlay.overlays.default ];
        };

        rustToolchain = pkgs.rust-bin.stable.latest.default.override {
          targets = [ "wasm32-wasip1" ];
        };

        buildScript = pkgs.writeShellScriptBin "build" ''
          cargo build --release --target wasm32-wasip1
        '';

      in
      {
        devShells.default = pkgs.mkShell {
          name = "zellij-pane-toggle-dev";

          packages = [
            rustToolchain
            pkgs.rust-analyzer
            buildScript
          ];

          CARGO_BUILD_TARGET = "wasm32-wasip1";

          shellHook = ''
            echo "zellij-pane-toggle dev shell"
            echo "  build   → cargo build --release  (or just 'build')"
            echo "  output  → target/wasm32-wasip1/release/zellij-pane-toggle.wasm"
          '';
        };

        packages.zellij-pane-toggle =
          let
            rustPlatform = pkgs.makeRustPlatform {
              cargo = rustToolchain;
              rustc = rustToolchain;
            };
          in
          rustPlatform.buildRustPackage {
            pname = "zellij-pane-toggle";
            version = "0.1.0";
            src = ./.;

            cargoLock.lockFile = ./Cargo.lock;

            buildPhase = ''
              cargo build --release --target wasm32-wasip1
            '';
            installPhase = ''
              mkdir -p $out/lib
              cp target/wasm32-wasip1/release/zellij-pane-toggle.wasm $out/lib/
            '';
            doCheck = false;
          };

        packages.default = self.packages.${system}.zellij-pane-toggle;
      }
    );
}
