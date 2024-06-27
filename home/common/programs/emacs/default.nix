{
  config,
  pkgs,
  ...
}: let
  c = config.colorscheme.palette;
  user = config.home.username;
in {
  home.persistence = {
    "/persist/home/${config.home.username}" = {
      directories = [".local/cache/emacs"];
      allowOther = true;
    };
  };

  programs.emacs = {
    enable = true;
    #package = pkgs.emacs;
    package =
      if user == "flakes"
      then pkgs.emacsPgtk
      else pkgs.emacs;
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

      (setq initial-scratch-message nil)  ; apparently fix for wayland gtk
      (setq use-dialog-box nil)
      (setq inhibit-startup-screen t)
      (setq package-check-signature nil)
      (set-frame-font "monospace 12" nil t)

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

      (setq user-emacs-directory (expand-file-name "./.config/emacs/"))

      (let ((backup-dir "~/.local/cache/emacs/backups")
            (auto-saves-dir "~/.local/cache/emacs/auto-saves/"))
        (dolist (dir (list backup-dir auto-saves-dir))
          (when (not (file-directory-p dir))
            (make-directory dir t)))
        (setq backup-directory-alist `(("." . ,backup-dir))
              auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
              auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
              tramp-backup-directory-alist `((".*" . ,backup-dir))
              tramp-auto-save-directory auto-saves-dir))

      (setq backup-by-copying t    ; Don't delink hardlinks
            delete-old-versions t  ; Clean up the backups
            version-control t      ; Use version numbers on backups,
            kept-new-versions 2    ; keep some new versions
            kept-old-versions 1)   ; and some old ones, too

      (require 'package):
      (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
      ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
      ;; and `package-pinned-packages`. Most users will not need or want to do this.
      ;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
      (package-initialize)
      (add-to-list 'package-archives
      '("MELPA" .
      "http://melpa.org/packages/"))
    '';
  };
  home.file = {
    ".config/emacs/init.el".source = pkgs.writeText "init.el" ''
      (setq user-emacs-directory (expand-file-name "./.config/emacs/"))
    '';
  };
}
