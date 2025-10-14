{
  pkgs,
  ...
}:
{
  #home.persistence = {
  #  "/persist" = {
  #    directories = [".local/cache/emacs"];
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

  home.packages = [
    pkgs.emacs-all-the-icons-fonts
  ];

  home.file = {
    ".local/share/applications/emacsclient.desktop".source = pkgs.writeText "emacsclient.desktop" ''
      [Desktop Entry]
      Name=Emacsclient
      GenericName=Text Editor
      Comment=Edit text
      MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;x-scheme-handler/org-protocol;
      Exec=${pkgs.emacs-pgtk}/bin/emacsclient -c -r %F
      Icon=emacs
      Type=Application
      Terminal=false
      Categories=Development;TextEditor;
      StartupNotify=true
      StartupWMClass=Emacs
      Keywords=emacsclient;
      Actions=new-window;new-instance;
    '';

    ".local/share/applications/emacs.desktop".source = pkgs.writeText "emacs.desktop" ''
      [Desktop Entry]
      Name=Emacs
      GenericName=Text Editor
      Comment=Edit text
      MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;x-scheme-handler/org-protocol;
      Exec=${pkgs.emacs-pgtk}/bin/emacsclient -c -r %F
      Icon=emacs
      Type=Application
      Terminal=false
      Categories=Development;TextEditor;
      StartupNotify=true
      StartupWMClass=Emacs
      Keywords=emacs;
      Actions=new-window;new-instance;
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
      epkgs.direnv
      epkgs.vertico
      epkgs.vertico-posframe
      epkgs.orderless
      epkgs.marginalia
      epkgs.embark
      epkgs.consult
      epkgs.consult-projectile
      epkgs.embark-consult
      epkgs.expand-region
      epkgs.nerd-icons-completion

      epkgs.treesit-auto
      epkgs.projectile
      epkgs.all-the-icons
      epkgs.all-the-icons-dired
      epkgs.vterm
      # epkgs.dired-single

      epkgs.org
      epkgs.org-node
      epkgs.org-modern

      epkgs.calfw
      epkgs.calfw-org
      epkgs.org-variable-pitch
      epkgs.olivetti
      epkgs.visual-fill-column
      epkgs.org-super-agenda

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
