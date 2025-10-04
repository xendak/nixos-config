{ pkgs, ... }:
{
  # imports = [ ./plugins ];
  home.packages = with pkgs; [
    yazi
  ];

  home.file.".config/yazi/keymap.toml" = pkgs.writeText ''
    [[mgr.prepend_keymap]]
    on   = ["f"]
    run  = "cd --interactive"
    desc = "Iteractively go to directory"

    [[mgr.prepend_keymap]]
    on   = ["F"]
    run  = "filter --smart"
    desc = "Iteractively go to directory"

    [[mgr.append_keymap]]
    on   = ["g", "p"]
    run = "cd ~/Programming"
    desc = "Go to Programming directory"

    [[mgr.append_keymap]]
    on   = ["g", "F"]
    run = "cd ~/Flake"
    desc = "Go to Flake directory"
  '';

  # home.sessionVariables = { };
}
