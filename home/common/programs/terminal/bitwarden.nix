{
  config,
  pkgs,
  ...
}: {
  home = {
    programs = [pkgs.bitwarden-cli];
    persistence = {
      "/persist/home/${config.home.username}" = {
        directories = [".config/Bitwarden CLI"];
        allowOther = true;
      };
    };
  };
}
