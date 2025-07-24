{
  config,
  pkgs,
  ...
}:
let
  mkEmacsTheme =
    { name, colorscheme }:
    pkgs.writeText "${name}-theme.el" ''
      (require 'base16-theme)

      (defvar base16-${name}-theme-colors
        '(:base00 "${colorscheme.palette.base00}"
          :base01 "${colorscheme.palette.base01}"
          :base02 "${colorscheme.palette.base02}"
          :base03 "${colorscheme.palette.base03}"
          :base04 "${colorscheme.palette.base04}"
          :base05 "${colorscheme.palette.base05}"
          :base06 "${colorscheme.palette.base06}"
          :base07 "${colorscheme.palette.base07}"
          :base08 "${colorscheme.palette.base08}"
          :base09 "${colorscheme.palette.base09}"
          :base0A "${colorscheme.palette.base0A}"
          :base0B "${colorscheme.palette.base0B}"
          :base0C "${colorscheme.palette.base0C}"
          :base0D "${colorscheme.palette.base0D}"
          :base0E "${colorscheme.palette.base0E}"
          :base0F "${colorscheme.palette.base0F}")
        "Colors for base16-${name} theme.")

      (deftheme base16-${name})
      (base16-theme-define 'base16-${name} base16-${name}-theme-colors)

      (custom-theme-set-faces 'base16-${name}
        '(mode-line ((t (:background "${colorscheme.palette.base02}" :foreground "${colorscheme.palette.base05}"))))
        '(mode-line-inactive ((t (:background "${colorscheme.palette.base01}" :foreground "${colorscheme.palette.base05}")))))

      (provide-theme 'base16-${name})
      (provide 'base16-${name}-theme)
    '';

  # Assume your themes are defined like this in your config
  # config.themes.default.colorScheme, config.themes.light.colorScheme, etc.
  defaultTheme = mkEmacsTheme {
    name = "default";
    colorscheme = config.themes.default.colorScheme;
  };
  lightTheme = mkEmacsTheme {
    name = "light";
    colorscheme = config.themes.light.colorScheme;
  };
  darkTheme = mkEmacsTheme {
    name = "dark";
    colorscheme = config.themes.dark.colorScheme;
  };

in
{
  #home.persistence = {
  #  "/persist/home/${config.home.username}" = {
  #    directories = [".local/cache/emacs"];
  #    allowOther = true;
  #  };
  #};

  imports = [
    ./init.nix
  ];

  services.emacs = {
    enable = true;
    # client.enable = true;
    socketActivation.enable = true;
    startWithUserSession = "graphical";
  };

  home.file = {
    ".config/emacs/themes/base16-default-theme.el".source = defaultTheme;
    ".config/emacs/themes/base16-light-theme.el".source = lightTheme;
    ".config/emacs/themes/base16-dark-theme.el".source = darkTheme;
    # easier to make quick/tryout changes
    # ".config/emacs/binds.el".source = ./binds.el;
    # ".config/emacs/config.el".source = ./config.el;
    # ".config/emacs/init.el".source = ./init.el;

    ".local/share/applications/emacsclient.desktop".source = pkgs.writeText "emacsclient.desktop" ''
      [Desktop Entry]
      Name=Emacsclient
      GenericName=Text Editor
      Comment=Edit text
      MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;x-scheme-handler/org-protocol;
      Exec=sh -c "if [ -n \\"\\$*\\" ]; then exec ${pkgs.emacs-pgtk}/bin/emacsclient --alternate-editor= --reuse-frame \\"\\$@\\"; else exec ${pkgs.emacs-pgtk}/bin/emacsclient --alternate-editor= --create-frame; fi" sh %F
      NoDisplay=true
      Hidden=true
      Icon=emacs
      Type=Application
      Terminal=false
      Categories=Development;TextEditor;
      StartupNotify=true
      StartupWMClass=Emacs
      Keywords=emacsclient;
      Actions=new-window;new-instance;

      [Desktop Action new-window]
      Name=New Window
      Exec=${pkgs.emacs-pgtk}/bin/emacsclient --alternate-editor= --create-frame %F

      [Desktop Action new-instance]
      Name=New Instance
      Exec=${pkgs.emacs-pgtk}/bin/emacs %F
    '';

    ".local/share/applications/emacs.desktop".source = pkgs.writeText "emacs.desktop" ''
      [Desktop Entry]
      Name=Emacs
      GenericName=Text Editor
      Comment=Edit text
      MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;x-scheme-handler/org-protocol;
      Exec=sh -c "if [ -n \\"\\$*\\" ]; then exec ${pkgs.emacs-pgtk}/bin/emacsclient --alternate-editor= --reuse-frame \\"\\$@\\"; else exec ${pkgs.emacs-pgtk}/bin/emacsclient --alternate-editor= --create-frame; fi" sh %F
      Icon=emacs
      Type=Application
      Terminal=false
      Categories=Development;TextEditor;
      StartupNotify=true
      StartupWMClass=Emacs
      Keywords=emacsclient;
      Actions=new-window;new-instance;

      [Desktop Action new-window]
      Name=New Window
      Exec=${pkgs.emacs-pgtk}/bin/emacsclient --alternate-editor= --create-frame %F

      [Desktop Action new-instance]
      Name=New Instance
      Exec=${pkgs.emacs-pgtk}/bin/emacs %F
    '';
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
    extraPackages = epkgs: [
      epkgs.magit
      epkgs.editorconfig
      epkgs.meow
      epkgs.devdocs
      epkgs.use-package
      epkgs.fzf
      epkgs.rg
      epkgs.rainbow-delimiters
      epkgs.base16-theme
      epkgs.spacious-padding
      epkgs.indent-bars
      epkgs.which-key

      epkgs.treesit-auto
      epkgs.projectile
      epkgs.all-the-icons
      epkgs.all-the-icons-dired

      epkgs.nix-mode
      epkgs.rust-mode
      epkgs.zig-mode
      epkgs.python-mode

      epkgs.lsp-java
      epkgs.lsp-mode
      epkgs.lsp-ui
      epkgs.company
    ];
  };
}
