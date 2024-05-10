{config, ...}: {
  programs.zoxide.enable = true;
  home.persistence = {
    "/persist/home/${config.home.username}" = {
      directories = [".local/share/zoxide"];
      allowOther = true;
    };
  };
}
