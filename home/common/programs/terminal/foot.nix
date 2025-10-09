{
  config,
  ...
}:
{
  # optional
  # home = {
  #   packages = [
  #     xterm.foot
  #   ];
  #   sessionVariables = {
  #     TERMINAL = lib.mkForce "foot";
  #   };
  #   sessionPath = ["$HOME/Flake/bin"];
  # };

  programs.foot = {
    enable = true;
    settings = {
      main = {
        font = "${config.fontProfiles.monospace.family}:size=12";
        pad = "15x15";
        term = "xterm-256color";
        include = "/home/${config.home.username}/.config/foot/colors.ini";
      };
    };
  };
}
