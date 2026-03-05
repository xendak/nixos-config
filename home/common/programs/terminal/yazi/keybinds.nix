{ ... }:
{
  programs.yazi.keymap = {
    mgr.prepend_keymap = [
      {
        on = "F";
        run = "filter --smart";
        desc = "Iteractively go to directory";
      }
      {
        on = "z";
        run = "plugin zoxide";
        desc = "Zoxide go to directory";
      }
      {
        on = "Z";
        run = "plugin fzf";
        desc = "Fzf go to directory";
      }
      {
        on = [
          "g"
          "p"
        ];
        run = "cd ~/Programming";
        desc = "Go to Programming directory";
      }
      {
        on = [
          "g"
          "F"
        ];
        run = "cd ~/Flake";
        desc = "Go to Flake directory";
      }
      {
        on = [
          "Y"
          "x"
        ];
        run = "unyank";
        desc = "Unyank";
      }
      {
        on = "@";
        run = ''shell "$SHELL" --block'';
        desc = "Enter shell in PWD";
      }
      {
        on = "<C-s>";
        for = "unix";
        run = ''shell "$SHELL" --block'';
        desc = "Open shell here";
      }

      # PLUGINS
      {
        on = "f";
        run = "plugin smart-filter";
        desc = "Iteractively go to directory";
      }
      {
        on = [
          "Y"
          "y"
        ];
        run = "plugin copy-file-contents content";
        desc = "Copy file(s) content only";
      }
      {
        on = [
          "Y"
          "a"
        ];
        run = "plugin copy-file-contents formatted";
        desc = "Copy file(s) content only in md format";
      }
      {
        on = ";";
        run = "plugin quickshell";
        desc = "Quickshell for hovered or selected items";
      }

      # plugin augment-command
      {
        on = "i";
        run = "plugin augment-command -- shell --block 'bat -p --pager $PAGER %s'";
        desc = "Open with bat";
      }
      {
        on = "<Enter>";
        run = "plugin augment-command -- enter";
        desc = "Enter the child directory, or open the file";

      }
      {
        on = "<S-Enter>";
        run = "plugin augment-command -- enter --interactive";
        desc = "Enter the child directory, or open the file";

      }
      {
        on = "o";
        run = "plugin augment-command -- open --interactive";
        desc = "Enter the child directory, or open the file";
      }
      {
        on = "O";
        run = "plugin augment-command -- open";
        desc = "Enter the child directory, or open the file";
      }
      {
        on = "A";
        run = "plugin augment-command -- archive";
        desc = "Add files to an archive";
      }
      {
        on = "+";
        run = "plugin augment-command -- create";
        desc = "Create a file or directory";
      }

      # plugin duckdb
      {
        on = "H";
        run = "plugin duckdb -1";
        desc = "Scroll one column to the left";
      }
      {
        on = "L";
        run = "plugin duckdb +1";
        desc = "Scroll one column to the right";
      }
    ];
  };
}
