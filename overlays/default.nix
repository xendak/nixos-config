# This file defines overlays
{ inputs, ... }:
{
  # This one brings our custom packages from the 'pkgs' directory
  additions = final: _prev: import ../pkgs { pkgs = final; };

  # This one contains whatever you want to overlay
  # You can change versions, add patches, set compilation flags, anything really.
  # https://nixos.wiki/wiki/Overlays
  modifications = final: prev: {
    # example = prev.example.overrideAttrs (oldAttrs: rec {
    # ...
    # });

    # Fixes missing mime associations/Open With menus in Dolphin on KDE 6 updates
    kdePackages = prev.kdePackages.overrideScope (
      kfinal: kprev:
      let
        kservice5Menu = prev.stdenv.mkDerivation {
          name = "kservice5-applications-menu";
          version = "5.116.0";

          src = prev.fetchFromGitLab {
            domain = "invent.kde.org";
            owner = "frameworks";
            repo = "kservice";
            tag = "v5.116.0";
            sparseCheckout = [ "src/applications.menu" ];
            hash = "sha256-28ueuJiI34o1wayiq85KPNkUCwjdhPMYtU2nJTQ84V4=";
          };

          installPhase = ''
            mkdir -p $out/etc/xdg/menus
            cp ./src/applications.menu $out/etc/xdg/menus/applications.menu
          '';
        };
      in
      {
        dolphin = prev.symlinkJoin {
          name = "dolphin-wrapped";
          paths = [ kprev.dolphin ];
          nativeBuildInputs = [ prev.makeWrapper ];

          postBuild = ''
            rm $out/bin/dolphin
            makeWrapper ${kprev.dolphin}/bin/dolphin $out/bin/dolphin \
              --prefix XDG_CONFIG_DIRS : "${kservice5Menu}/etc/xdg" \
              --run "${kprev.kservice}/bin/kbuildsycoca6 --noincremental ${kservice5Menu}/etc/xdg/menus/applications.menu"
          '';
        };
      }
    );

    openldap = prev.openldap.overrideAttrs {
      doCheck = !prev.stdenv.hostPlatform.isi686;
    };
    avrdude = prev.avrdude.override {
      docSupport = false;
    };

    zls-overlay =
      inputs.zls.packages.${final.stdenv.hostPlatform.system}.default.overrideAttrs
        (oldAttrs: {
          doCheck = false;
          doInstallCheck = false;
          nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ [
            inputs.zig.packages.${final.stdenv.hostPlatform.system}."0.16.0"
          ];
        });
    zig-master = inputs.zig.packages.${final.stdenv.hostPlatform.system}."0.16.0";

    # niri = prev.niri.override { libdisplay-info = final.libdisplay-info_0_3; };
    niri =
      let
        # Define src once so we can pass it to both the derivation and cargoDeps
        src = final.fetchFromGitHub {
          owner = "MithicSpirit";
          repo = "niri";
          rev = "1352ddeefa7a2687d5f7babd346abe878625c3a1";
          hash = "sha256-a0lM4jaIs5HppfEq0OudrXbvAw2bXlvAv81QAJ8Ex+c=";
        };
      in
      (prev.niri).overrideAttrs (oldAttrs: {
        inherit src;
        version = "force-render-v2-2026-08-09";

        # Override cargoDeps instead of cargoHash
        cargoDeps = final.rustPlatform.fetchCargoVendor {
          inherit src;
          hash = "sha256-HypBB3PL4nVFMNH2+jEK0+dG9dJ920nHi8GwRoeH/v4=";
        };
        doInstallCheck = false;
      });
  };

  stable = final: _: {
    stable = inputs.nixpkgs-stable.legacyPackages.${final.stdenv.hostPlatform.system};
  };

  # When applied, the unstable nixpkgs set (declared in the flake inputs) will
  # be accessible through 'pkgs.unstable'
  unstable-packages = final: _prev: {
    unstable = import inputs.nixpkgs-unstable {
      system = final.stdenv.hostPlatform.system;
      config.allowUnfree = true;
    };
  };
}
