{
  description = "Zig Project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    zig-overlay.url = "github:mitchellh/zig-overlay";
    zls-overlay.url = "github:zigtools/zls";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    zig-overlay,
    zls-overlay,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [zig-overlay.overlays.default];
      };

      pkgsCross = import nixpkgs {
        inherit system;
        crossSystem = pkgs.lib.systems.examples.mingwW64;
      };

      zig = zig-overlay.packages.${system}.master;
      zls = zls-overlay.packages.${system}.default.override {
        inherit zig;
      };
    in {
      packages.default = pkgs.callPackage ./default.nix {inherit zig;};

      packages.windows = pkgsCross.callPackage ./default.nix {
        zig = zig.overrideAttrs (oa: {
          preConfigure = ''
            export NIX_CFLAGS_COMPILE="-target x86_64-windows-gnu $NIX_CFLAGS_COMPILE"
          '';
        });
      };

      devShells.default = pkgs.mkShell {
        nativeBuildInputs = with pkgs; [
          zig
          zls
          # Additional development tools
          lldb
          llvmPackages_latest.lld
          git
          gdb
          valgrind

          # Windows
          pkgsCross.windows.mingw_w64_pthreads
        ];

        shellHook = ''
          echo "Zig ${zig.version} environment"
        '';
      };

      hydraJobs = self.packages.${system};
    });
}
