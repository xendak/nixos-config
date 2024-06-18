{pkgs, ...}: {
  home.programs = with pkgs; [
    wezterm
    yazi
    foot
  ];
}
