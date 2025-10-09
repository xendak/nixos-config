{
  inputs,
  ...
}:
{
  # TODO: next release includes the "include" directive,
  # which allows to fix my config colors, in a simple way.
  imports = [
    inputs.niri.homeModules.niri
    ./settings.nix
    ./keybinds.nix
    ./rules.nix
    ./autostart.nix
  ];
}
