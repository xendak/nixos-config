{ pkgs, config, ... }:
{
  programs.password-store = {
    enable = true;
    settings = {
      PASSWORD_STORE_DIR = "$XDG_CONFIG_HOME/.password-store";
    };
    package = pkgs.pass.withExtensions (p: [p.pass-otp]);
  };

  home.persistence = {
    "/persist/home/${config.home.username}/.config".directories = [".password-store"];
  };
}
