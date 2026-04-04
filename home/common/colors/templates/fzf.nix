{ paletteSet, lib, ... }:
let
  p = paletteSet.palette;
  fzfColors = lib.concatStringsSep "," [
    "bg:${p.background}"
    "bg+:${p.surface_container_high}"
    "fg:${p.foreground}"
    "fg+:${p.foreground}"

    "hl:${p.primary}:bold:underline"
    "hl+:${p.primary}:bold:underline"

    "prompt:${p.cursor_bg}"
    "pointer:${p.primary}"
    "info:${p.magenta}"
    "spinner:${p.green}"
  ];
in
{
  "fzf/colors" = "--color='${fzfColors}' --ansi --layout=reverse --border";
}
