{ paletteSet, ... }:
let
  p = paletteSet.palette;
  # name = paletteSet.slug;
  name = "nix";
in
{
  "emacs/themes/base16-${name}-theme.el" = # elisp
    ''
      (require 'base16-theme)
      (defvar base16-${name}-theme-colors
        '(:base00 "${p.base00}"
          :base01 "${p.base01}"
          :base02 "${p.base02}"
          :base03 "${p.base03}"
          :base04 "${p.base04}"
          :base05 "${p.base05}"
          :base06 "${p.base06}"
          :base07 "${p.base07}"
          :base08 "${p.base08}"
          :base09 "${p.base09}"
          :base0A "${p.base0A}"
          :base0B "${p.base0B}"
          :base0C "${p.base0C}"
          :base0D "${p.base0D}"
          :base0E "${p.base0E}"
          :base0F "${p.base0F}")
        "Colors for base16-${name} theme.")

      (deftheme base16-${name})
      (base16-theme-define 'base16-${name} base16-${name}-theme-colors)

      (custom-theme-set-faces 'base16-${name}
        '(mode-line ((t (:background "${p.base02}" :foreground "${p.base05}"))))
        '(mode-line-inactive ((t (:background "${p.base01}" :foreground "${p.base05}")))))

      (provide-theme 'base16-${name})
      (provide 'base16-${name}-theme)
    '';
  # "emacs/themes/current-theme.el" = # elisp
  #   ''
  #     (load-theme 'base16-nix t)
  #   '';
}
