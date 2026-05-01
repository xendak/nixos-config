{ paletteSet, ... }:
let
  p = paletteSet.palette;
in
{
  "zellij/default.kdl" =
    # kdl
    ''
      themes {
          default {
              fg "${p.foreground}"
              bg "${p.background}"
              black "${p.black}"
              red "${p.red}"
              green "${p.green}"
              yellow "${p.yellow}"
              blue "${p.blue}"
              magenta "${p.magenta}"
              cyan "${p.cyan}"
              white "${p.white}"
              orange "${p.orange}"
          }
      }
      theme "default"
    '';
}
