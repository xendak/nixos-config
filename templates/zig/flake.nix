{
  description = "Zig Project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    zig.url = "github:mitchellh/zig-overlay";
    zls.url = "github:zigtools/zls";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    zig,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [zig.overlays.master];
      };

      zigPackage = zig.packages.${system}.master;
    in {
      packages.default = pkgs.callPackage ./default.nix {inherit zig;};

      devShells.default = pkgs.mkShell {
        nativeBuildInputs = with pkgs; [
          zigPackage
          inputs.zls."{system}".master
          
          # Additional development tools
          raylib
          lldb
          llvmPackages_latest.lld
          git
          gdb
          valgrind
        ];

        shellHook = ''
          echo "Zig ${zig.version} environment"
        '';
      };

      hydraJobs = self.packages.${system};
    });
}
