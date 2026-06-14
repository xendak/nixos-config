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
