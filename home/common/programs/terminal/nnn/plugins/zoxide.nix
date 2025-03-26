{pkgs, ...}: {
  xdg.configFile."nnn/plugins/zoxide".source = pkgs.writeShellScript "zoxide" ''
    # Description: Navigate to directory using jump/autojump/zoxide/z
    #
    # Dependencies:
    #   - zoxide - https://github.com/ajeetdsouza/zoxide
    #
    # Note: The dependencies STORE NAVIGATION PATTERNS
    #
    # to make z.lua work, you need to set $NNN_ZLUA to the path of script z.lua
    #
    # Shell: POSIX compliant
    # Authors: Marty Buchaus, Dave Snider, Tim Adler, Nick Waywood

    if [ ! -p "$NNN_PIPE" ]; then
        printf 'ERROR: NNN_PIPE is not set!'
        read -r _
        exit 2
    fi

    if type zoxide >/dev/null 2>&1; then
        if type fzf >/dev/null 2>&1; then
          odir="$(zoxide query -i --)"
          printf "%s" "0c$odir" > "$NNN_PIPE"
        else
      printf "jump to : "
          read -r dir
          odir="$(zoxide query -- "$dir")"
          printf "%s" "0c$odir" > "$NNN_PIPE"
        fi
    fi
  '';
}
