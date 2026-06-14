{
  pkgs,
  inputs,
  ...
}:
let
  pkgs-pr = import inputs.discord-pr {
    inherit (pkgs.stdenv.hostPlatform) system;
    config.allowUnfree = true;
  };
in
{
  home.packages = [
    (pkgs-pr.discord-canary.override {
      nss = pkgs.nss_latest;
      withOpenASAR = true;
    })
  ];

  home.persistence = {
    "/persist" = {
      directories = [ ".config/discordcanary" ];
    };
  };
}
