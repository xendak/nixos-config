{
  pkgs,
  ...
}:
{
  home.packages = [
    (pkgs.discord-canary.override {
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
