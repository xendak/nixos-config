{ pkgs, ... }:
{
  programs.fish = {
    enable = true;
    shellAbbrs = {

      jqless = "jq -C | less -r";

      nd = "nix develop -c $SHELL";
      ns = "nix shell";
      nsn = "nix shell nixpkgs#";
      nb = "nix build";
      nbn = "nix build nixpkgs#";
      nf = "nix flake";

      nr = "nixos-rebuild --flake .";
      nrs = "nixos-rebuild --flake . switch";
      snr = "sudo nixos-rebuild --flake .";
      snrs = "sudo nixos-rebuild --flake . switch";
      #hm = "home-manager --flake .";
      #hms = "home-manager --flake . switch";
      hm = "home";
      ninit = "nix flake init -t $HOME/Flake#";

      nv = "nvim";
      nvi = "nvim";
      v = "nvim";
      vi = "nvim";
      vim = "nvim";

      ll = "eza -lah";
      ls = "eza";
      la = "eza -a";
      exa = "eza";

      # rpr = "cd $HOME/Programming/Rust";
    };

    shellAliases = {
      upd = "sh $HOME/Flake/deploy.sh";
      upb = "sh $HOME/Flake/boot-deploy.sh";
      upn = "cp $HOME/Flake/flake.lock $HOME/Flake/flake.lock.old && nix flake update";
      # Get ip
      getip = "curl ifconfig.me";
      # SSH with kitty terminfo
      kssh = "kitty +kitten ssh";
      # Clear screen and scrollback
      clear = "printf '\\033[2J\\033[3J\\033[1;1H'";
    };

    functions = {
      fish_greeting = "";
      home = "cd ~/";
      wh = "readlink -f (which $argv)";
      kb = "
        if test (count $argv) -lt 1; or test $argv[1] = \"help\"
          echo \"Valid Options: FIXED 2
                  m or moonlander -> cd to kb/moonlander/km/xendak 
                  ap or annepro   -> cd to kb/annepro2/km/xendak
                  c or compile    -> specify keyboard to compile options ap m annepro moonlander
                  f or flash      -> same as above, but for flashing utility to use
                  cf or fc        -> same as above, but for both utility to use\"
        else
          if test $argv[1] = \"m\"; or test $argv[1] = \"moonlander\"
            cd $HOME/Programming/Keyboard/qmk_firmware/keyboards/zsa/moonlander/keymaps/xendak
          else if test $argv[1] = \"ap\"; or test $argv[1] = \"annepro\"
            cd $HOME/Programming/Keyboard/qmk_firmware/keyboards/annepro2/keymaps/xendak
          else if test $argv[1] = \"compile\"; or test $argv[1] = \"c\"
            if test $argv[2] = \"m\"; or test $argv[2] = \"moonlander\"
              qmk compile -kb moonlander -km xendak
            else if test $argv[2] = \"ap\"; or test $argv[2] = \"annepro\"
              qmk compile -kb annepro2 -km xendak
            else
              echo \"Valid Options:
                      m or moonlander -> compiles kb/moonlander/km/xendak 
                      ap or annepro   -> compiles kb/annepro2/km/xendak
                  \"
            end
          else if test $argv[1] = \"flash\"; or test $argv[1] = \"f\"
            if test $argv[2] = \"m\"; or test $argv[2] = \"moonlander\"
              sudo wally-cli $HOME/Programming/Keyboard/qmk_firmware/zsa_moonlander_xendak.bin
            else if test $argv[2] = \"ap\"; or test $argv[2] = \"annepro\"
              echo \"gotta remember to specify this later\"
            else
              echo \"Valid Options:
                      m or moonlander -> flashes kb/moonlander/km/xendak 
                      ap or annepro   -> flashes kb/annepro2/km/xendak
                  \"
            end
          else if test $argv[1] = \"cf\"; or test $argv[1] = \"fc\"
            if test $argv[2] = \"m\"; or test $argv[2] = \"moonlander\"
              qmk compile -kb moonlander -km xendak && sudo wally-cli $HOME/Programming/Keyboard/qmk_firmware/zsa_moonlander_xendak.bin
            else if test $argv[2] = \"ap\"; or test $argv[2] = \"annepro\"
              echo \"gotta remember to specify this later\"
            else
              echo \"Valid Options:
                      m or moonlander -> compiles and flashes kb/moonlander/km/xendak 
                      ap or annepro   -> compiles and flashes kb/annepro2/km/xendak
                  \"
            end
          end
        end
      ";
      fish_mode_prompt = "";
      fish_prompt = "
        set -l last_status $status

        if not set -q __fish_prompt_hostname
        set -g __fish_prompt_hostname (hostnamectl|cut -d: -f 2)
        end

       # Setup colors
       # Fish Vi Mode
        set -l red (set_color red)
        if test $fish_bind_mode = \"default\"
          set red (set_color red)
        else if test $fish_bind_mode = \"insert\"
          set red (set_color green)
        end
        set -l normal (set_color normal)
        set -l cyan (set_color cyan)
        set -l white (set_color normal)

        # Configure __fish_git_prompt
        set -g __fish_git_prompt_char_stateseparator ' '
        set -g __fish_git_prompt_color normal
        set -g __fish_git_prompt_color_flags red
        set -g __fish_git_prompt_color_prefix cyan
        set -g __fish_git_prompt_color_suffix cyan
        set -g __fish_git_prompt_showdirtystate true
        set -g __fish_git_prompt_showuntrackedfiles false
        set -g __fish_git_prompt_showstashstate true
        set -g __fish_git_prompt_show_informative_status false

        # Line 1
        echo -n $cyan'┌─('$red'^.^'$cyan')'$white'-'$cyan'('$white(prompt_pwd)$cyan')'
        __fish_git_prompt \"-[git://%s]-\"
        echo

        # Line 2
        echo -n $cyan'└─O'  $normal
      ";
      n = {
        wraps = "nnn";
        body = "
          if test -n \"$NNNLVL\" -a \"$NNNLVL\" -ge 1
            echo \"nnn is already running\"
            return
          end

          if test -n \"$XDG_CONFIG_HOME\"
              set -x NNN_TMPFILE \"$XDG_CONFIG_HOME/nnn/.lastd\"
          else
              set -x NNN_TMPFILE \"$HOME/.config/nnn/.lastd\"
          end
          command nnn -deiH $argv

          if test -e $NNN_TMPFILE
              source $NNN_TMPFILE
              rm $NNN_TMPFILE
          end
        ";
      };
    };

    interactiveShellInit =
      # Open command buffer in vim when alt+e is pressed
      ''
        bind \ee edit_command_buffer
      '' +
      # nnn integration
      '' 
        set -gx NNN_FCOLORS "030201050006060009060402"
      '' +
      # kitty integration
      ''
        set --global KITTY_INSTALLATION_DIR "${pkgs.kitty}/lib/kitty"
        set --global KITTY_SHELL_INTEGRATION enabled
        source "$KITTY_INSTALLATION_DIR/shell-integration/fish/vendor_conf.d/kitty-shell-integration.fish"
        set --prepend fish_complete_path "$KITTY_INSTALLATION_DIR/shell-integration/fish/vendor_completions.d"
      '' +
      # Use vim bindings and cursors
      ''
        fish_vi_key_bindings
        set fish_cursor_default     block      blink
        set fish_cursor_insert      line       blink
        set fish_cursor_replace_one underscore blink
        set fish_cursor_visual      block
      '' +
      # Use terminal colors
      ''
        set -U fish_color_autosuggestion      brblack
        set -U fish_color_cancel              -r
        set -U fish_color_command             brgreen
        set -U fish_color_comment             brmagenta
        set -U fish_color_cwd                 green
        set -U fish_color_cwd_root            red
        set -U fish_color_end                 brmagenta
        set -U fish_color_error               brred
        set -U fish_color_escape              brcyan
        set -U fish_color_history_current     --bold
        set -U fish_color_host                normal
        set -U fish_color_match               --background=brblue
        set -U fish_color_normal              normal
        set -U fish_color_operator            cyan
        set -U fish_color_param               brblue
        set -U fish_color_quote               yellow
        set -U fish_color_redirection         bryellow
        set -U fish_color_search_match        'bryellow' '--background=brblack'
        set -U fish_color_selection           'white' '--bold' '--background=brblack'
        set -U fish_color_status              red
        set -U fish_color_user                brgreen
        set -U fish_color_valid_path          --underline
        set -U fish_pager_color_completion    normal
        set -U fish_pager_color_description   yellow
        set -U fish_pager_color_prefix        'white' '--bold' '--underline'
        set -U fish_pager_color_progress      'brwhite' '--background=cyan'
      '';
  };
}
