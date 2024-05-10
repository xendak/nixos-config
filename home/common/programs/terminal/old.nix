{
  pkgs,
  lib,
  ...
}:
# old prompt
# $username$hostname($shlvl)($cmd_duration) $fill ($nix_shell)$custom
# $directory(${git})(${cloud}) $fill $time
# $jobs$character
# sym ᐉ
{
  programs.starship = {
    enable = true;
    settings = {
      format = let
        git = "  -> $git_branch$git_commit$git_state$git_status";
        cloud = "  -> $aws$gcloud$openstack";
      in ''
         $jobs  $hostname$username(${git})(${cloud})($cmd_duration)
        ╭─[](yellow)[ ($nix_shell)($rust)($c)](bg:yellow fg:black)[](fg:black bg:yellow)$directory[](yellow) $fill
        ╰─$character
      '';
      fill = {
        symbol = " ";
        disabled = false;
      };

      # Core
      username = {
        format = "[$user]($style)";
        show_always = true;
      };
      hostname = {
        format = "[$hostname@]($style)";
        ssh_only = false;
        style = "bold green";
      };
      shlvl = {
        format = "[$shlvl]($style) ";
        style = "bold cyan";
        threshold = 2;
        repeat = true;
        disabled = false;
      };
      cmd_duration = {
        format = " -> took [$duration]($style) ";
      };
      directory = {
        truncation_length = 5;
        style = "fg:black bg:yellow";
        format = "[   $path ]($style)";
        #format = "[$path]($style)( [$read_only]($read_only_style)) ";
      };
      nix_shell = {
        #format = "[($name \\(develop\\) <- )$symbol]($style) ";
        format = "[$symbol]($style) ";
        impure_msg = "";
        symbol = " ";
        style = "bg:yellow fg:bold black";
      };
      custom = {
        nix_inspect = let
          excluded = ["kitty" "imagemagick" "ncurses" "user-environment"];
        in {
          disabled = false;
          when = "test -z $IN_NIX_SHELL";
          command = "${(lib.getExe pkgs.nix-inspect)} ${(lib.concatStringsSep " " excluded)}";
          format = "[($output <- )$symbol]($style) ";
          symbol = " ";
          style = "bold blue";
        };
      };

      character = {
        error_symbol = "[ᐉ ](bold red)";
        success_symbol = "[ᐉ ](bold blue)";
        vimcmd_symbol = "[ᐉ ](bold green)";
        #vimcmd_symbol = "[ᐉ ](bold yellow)";
        vimcmd_visual_symbol = "[ᐉ ](bold cyan)";
        vimcmd_replace_symbol = "[ᐉ ](bold purple)";
        vimcmd_replace_one_symbol = "[ᐉ ](bold blue)";
      };

      time = {
        format = "\\\[[$time]($style)\\\]";
        disabled = false;
      };

      # Cloud
      gcloud = {
        format = "on [$symbol$active(/$project)(\\($region\\))]($style)";
      };
      aws = {
        format = "on [$symbol$profile(\\($region\\))]($style)";
        style = "bold yellow";
      };

      git_branch.style = "bold green";
      git_commit.style = "bold green";
      git_state.style = "bold green";
      git_status.style = "bold green";

      # style change try
      c = {
        format = "[$symbol]($style)";
        style = "bg:yellow fg:bold black";
      };

      haskell = {
        format = "[$symbol]($style)";
        style = "bg:yellow fg:bold black";
      };

      rust = {
        format = "[$symbol]($style)";
        style = "bg:yellow fg:bold black";
      };

      # Icon changes only \/
      aws.symbol = " ";
      conda.symbol = " ";
      dart.symbol = " ";
      directory.read_only = " ";
      docker_context.symbol = " ";
      elm.symbol = " ";
      elixir.symbol = "";
      gcloud.symbol = " ";
      git_branch.symbol = " ";
      golang.symbol = " ";
      hg_branch.symbol = " ";
      java.symbol = " ";
      julia.symbol = " ";
      memory_usage.symbol = "󰍛 ";
      nim.symbol = "󰆥 ";
      nodejs.symbol = " ";
      package.symbol = "󰏗 ";
      perl.symbol = " ";
      php.symbol = " ";
      python.symbol = " ";
      ruby.symbol = " ";
      rust.symbol = " ";
      scala.symbol = " ";
      shlvl.symbol = "";
      swift.symbol = "󰛥 ";
      terraform.symbol = "󱁢";
    };
  };
}
