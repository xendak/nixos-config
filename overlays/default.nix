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
    # yuzu-ea = prev.yuzu-early-access.overrideAttrs (oldAttrs: {
    #   patches = (oldAttrs.patches or [ ]) ++ [ ./yuzu-no-error.patch ];
    # });
    #   patches = (oldAttrs.patches or [ ]) ++ [ ./nautilus-typeahead.patch ];
    # });
    xdg-utils-spawn-terminal = prev.xdg-utils.overrideAttrs (oldAttrs: {
      patches = (oldAttrs.patches or [ ]) ++ [ ./xdg-open-spawn-terminal.diff ];
    });

    material-symbols = prev.material-symbols.overrideAttrs (oldAttrs: {
      version = "4.0.0-unstable-2025-04-11";

      src = final.fetchFromGitHub {
        owner = "google";
        repo = "material-design-icons";
        rev = "941fa95d7f6084a599a54ca71bc565f48e7c6d9e";
        hash = "sha256-5bcEh7Oetd2JmFEPCcoweDrLGQTpcuaCU8hCjz8ls3M=";
        sparseCheckout = [ "variablefont" ];
      };
    });
  };

  stable = final: _: {
    stable = inputs.nixpkgs-stable.legacyPackages.${final.system};
  };

  # When applied, the unstable nixpkgs set (declared in the flake inputs) will
  # be accessible through 'pkgs.unstable'
  unstable-packages = final: _prev: {
    unstable = import inputs.nixpkgs-unstable {
      system = final.system;
      config.allowUnfree = true;
    };
  };
}
