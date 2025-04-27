{
  description = "Zig Project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    zig-overlay.url = "github:mitchellh/zig-overlay";
    zls-overlay.url = "github:zigtools/zls";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, zig-overlay, zls-overlay, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ zig-overlay.overlays.default ];
        };
        zig = zig-overlay.packages.${system}.master;
        zls = zls-overlay.packages.${system}.default.override {
          inherit zig;
        };
      in
      {
        packages.default = pkgs.callPackage ./default.nix { inherit zig; };

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
          ];

          shellHook = ''
            echo "Zig ${zig.version} environment"
          '';
        };

        hydraJobs = self.packages.${system};
      });
}
