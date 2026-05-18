{ paletteSet, lib, ... }:
let
  p = paletteSet.palette;
  osc = code: color: "\\033]${toString code};${color}\\007";
  oscColor = idx: color: osc "4;${toString idx}" color;
in
{
  # This will generate a file with literal \033]... strings
  "terminal/sequences.txt" = ''
    ${oscColor 0 p.black}
    ${oscColor 1 p.red}
    ${oscColor 2 p.yellow}
    ${oscColor 3 p.green}
    ${oscColor 4 p.blue}
    ${oscColor 5 p.magenta}
    ${oscColor 6 p.cyan}
    ${oscColor 7 p.white}
    ${oscColor 8 p.brightblack} 
    ${oscColor 9 p.brightred}
    ${oscColor 10 p.brightyellow}
    ${oscColor 11 p.brightgreen}
    ${oscColor 12 p.brightblue}
    ${oscColor 13 p.brightmagenta}
    ${oscColor 14 p.brightcyan}
    ${oscColor 15 p.brightwhite}
    ${osc 10 p.fg}
    ${osc 11 p.bg}
    ${osc 12 p.cursor_bg}
  '';
}
