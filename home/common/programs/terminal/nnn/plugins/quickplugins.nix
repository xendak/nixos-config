{ pkgs, ... }:
{
  xdg.configFile."nnn/plugins/rg".source = pkgs.writeShellScript "rg" ''
    . "$(dirname "$0")"/.nnn-plugin-helper

    printf "pattern: "
    read -r pattern

    if [ -n "$pattern" ]; then
        printf "%s" "+l" > "$NNN_PIPE"
        eval "rg -l0 --hidden -S $pattern" > "$NNN_PIPE"
    fi
  '';

  xdg.configFile."nnn/plugins/fd".source = pkgs.writeShellScript "fd" ''
    . "$(dirname "$0")"/.nnn-plugin-helper

    printf "pattern: "
    read -r pattern

    if [ -n "$pattern" ]; then
        printf "%s" "+l" > "$NNN_PIPE"
        eval "fd -HI $pattern -0" > "$NNN_PIPE"
    fi
  '';
}
