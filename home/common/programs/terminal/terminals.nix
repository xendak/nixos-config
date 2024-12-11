{pkgs, ...}: {
  home.packages = with pkgs; [
    # wezterm
    yazi
    foot
  ];
}
