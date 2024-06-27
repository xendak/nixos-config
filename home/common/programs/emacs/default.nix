{
  config,
  pkgs,
  ...
}: let
  c = config.colorscheme.palette;
in {
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    extraPackages = epkgs: [
      epkgs.magit
      epkgs.lsp-mode
      epkgs.nix-mode
      epkgs.rust-mode
      epkgs.python-mode
      epkgs.editorconfig
      epkgs.eglot
      epkgs.devdocs
      epkgs.use-package
      epkgs.fzf
      epkgs.rg
      epkgs.rainbow-delimiters
      (epkgs.trivialBuild {
        pname = "base16-stylix-theme";
        version = "0.1.0";
        src = pkgs.writeText "base16-stylix-theme.el" ''
          (require 'base16-theme)

          (defvar base16-stylix-theme-colors
            '(:base00 "#${c.base00}"
              :base01 "#${c.base01}"
              :base02 "#${c.base02}"
              :base03 "#${c.base03}"
              :base04 "#${c.base04}"
              :base05 "#${c.base05}"
              :base06 "#${c.base06}"
              :base07 "#${c.base07}"
              :base08 "#${c.base08}"
              :base09 "#${c.base09}"
              :base0A "#${c.base0A}"
              :base0B "#${c.base0B}"
              :base0C "#${c.base0C}"
              :base0D "#${c.base0D}"
              :base0E "#${c.base0E}"
              :base0F "#${c.base0F}")
            "All colors for Base16 stylix are defined here.")

          ;; Define the theme
          (deftheme base16-stylix)

          ;; Add all the faces to the theme
          (base16-theme-define 'base16-stylix base16-stylix-theme-colors)

          ;; Mark the theme as provided
          (provide-theme 'base16-stylix)

          ;; Add path to theme to theme-path
          (add-to-list 'custom-theme-load-path
              (file-name-directory
                  (file-truename load-file-name)))

          (provide 'base16-stylix-theme)
        '';
        packageRequires = [epkgs.base16-theme];
      })
    ];
    extraConfig = ''
      (require 'package)
      (require 'fzf)
      (require 'base16-stylix-theme)
      (load-theme 'base16-stylix t)

      (tab-bar-mode -1)
      (menu-bar-mode -1)
      (tool-bar-mode -1)
      (scroll-bar-mode -1)

      (setq inhibit-startup-screen t)
      (setq package-check-signature nil)
      (set-frame-font "monospace 14" nil t)

      (setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completion-ignore-case t)


      (use-package rainbow-delimiters
                   :ensure t
                   :hook ((lisp-mode emacs-lisp-mode scheme-mode hy-mode) . rainbow-delimiters-mode))

      (custom-set-variables
       '(package-selected-packages '(rainbow-delimiters all-the-icons-dired)))
      (custom-set-faces
       )


      (add-to-list 'package-archives
      '("MELPA" .
      "http://melpa.org/packages/"))
       (package-initialize)
    '';
  };
  home.file = {
    ".config/emacs/init.el".source = pkgs.writeText "init.el" ''
      (require 'package)
      (require 'fzf)
      (require 'base16-stylix-theme)
      (load-theme 'base16-stylix t)

      (tab-bar-mode -1)
      (menu-bar-mode -1)
      (tool-bar-mode -1)
      (scroll-bar-mode -1)

      (setq inhibit-startup-screen t)
      (setq package-check-signature nil)
      (set-frame-font "monospace 14" nil t)

      (setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completion-ignore-case t)


      (use-package rainbow-delimiters
                   :ensure t
                   :hook ((lisp-mode emacs-lisp-mode scheme-mode hy-mode) . rainbow-delimiters-mode))

      (custom-set-variables
       '(package-selected-packages '(rainbow-delimiters all-the-icons-dired)))
      (custom-set-faces
       )


      (add-to-list 'package-archives
      '("MELPA" .
      "http://melpa.org/packages/"))
    '';
  };
}
