{
  pkgs,
  lib,
  pluginBinds,
  ...
}:
{
  imports = [ ./plugins ];
  home.packages = [
    pkgs.yazi
    (pkgs._7zz.override { enableUnfree = true; })
  ];

  xdg.mimeApps.defaultApplications = {
    "inode/directory" = lib.mkForce [ "yazi.desktop" ];
  };

  home.file.".config/yazi/keymap.toml".source = (
    pkgs.writeText "keymap.toml"
      # toml
      ''
        [[mgr.prepend_keymap]]
        on   = ["F"]
        run  = "filter --smart"
        desc = "Iteractively go to directory"

        [[mgr.prepend_keymap]]
        on   = ["z"]
        run  = "plugin zoxide"
        desc = "Zoxide go to directory"

        [[mgr.prepend_keymap]]
        on   = ["Z"]
        run  = "plugin fzf"
        desc = "Fzf go to directory"

        [[mgr.append_keymap]]
        on   = ["g", "p"]
        run = "cd ~/Programming"
        desc = "Go to Programming directory"

        [[mgr.append_keymap]]
        on   = ["g", "F"]
        run = "cd ~/Flake"
        desc = "Go to Flake directory"

        [[mgr.prepend_keymap]]
        on   = ["Y", "x"]
        run  = "unyank"
        desc = "Unyank"

        [[mgr.prepend_keymap]]
        on = "<C-s>"
        for = "unix"
        run = 'shell "$SHELL" --block'
        desc = "Open shell here"

      ''
    + "\n"
    + pluginBinds
  );

}
