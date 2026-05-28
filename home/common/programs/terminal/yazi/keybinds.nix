{ pkgs, ... }:
{
  programs.yazi.keymap = {
    mgr.prepend_keymap = [
      {
        on = "M";
        run = "plugin sshfs -- menu";
        desc = "Open SSHFS options";
      }
      {
        on = [
          "m"
          "d"
        ];
        run = "linemode my_default";
        desc = "Linemode: custom";
      }
      {
        desc = "Rofi Filebrowser (Grid View)";
        run = ''shell -- ${pkgs.rofi}/bin/rofi -show-icons -theme fullscreen-preview -show filebrowser -preview-cmd "${pkgs.ffmpeg}/bin/ffmpeg -y -ss 00:00:05 -i \"{input}\" -vf \"scale={size}:-1\" -frames:v 1 \"{output}\"" -filebrowser-command "ya emit reveal" -filebrowser-directory "$(pwd)"'';
        on = "<C-g>";
      }
      {
        desc = "Select directory(ies), file(s)";
        on = "<C-o>";
        run = "open";
      }
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
      # zoxide-manager
      {
        desc = "Reset $PWD entry in zoxide";
        on = [
          "<A-z>"
          "r"
        ];
        run = "plugin zoxide-manager -- reset";
      }
      {
        desc = "Remove $PWD from zoxide";
        on = [
          "<A-z>"
          "d"
        ];
        run = "plugin zoxide-manager -- remove";
      }
      {
        desc = "Add $PWD to zoxide";
        on = [
          "<A-z>"
          "a"
        ];
        run = "plugin zoxide-manager -- add";
      }

      {
        on = "f";
        run = "plugin smart-filter";
        desc = "Iteractively go to directory";
      }
      {
        on = [
          "Y"
          "c"
        ];
        run = [
          ''shell -- for path in "$@"; do echo "file://$path"; done | ${pkgs.wl-clipboard}/bin/wl-copy -t text/uri-list''
          "yank"
        ];
        desc = "Yank files to Wayland clipboard and internal memory";
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
