{config, ...}: {
  programs.rbw = {
    enable = true;
    settings = {
      email = "rg.grossi@outlook.com";
    };
  };
  home = {
    persistence = {
      "/persist/home/${config.home.username}" = {
        directories = [".config/Bitwarden CLI"];
        allowOther = true;
      };
    };
  };
}
