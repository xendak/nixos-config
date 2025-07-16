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
      (provide-theme 'base16-${name})
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
  # add tihs to services for daemon
  services.emacs = {
    enable = true;
    package = pkgs.emacs; # replace with emacs-gtk, or a version provided by the community overlay if desired.
  };

  home.file = {
    ".config/emacs/themes/base16-default-theme.el".source = defaultTheme;
    ".config/emacs/themes/base16-light-theme.el".source = lightTheme;
    ".config/emacs/themes/base16-dark-theme.el".source = darkTheme;
    # easier to make quick/tryout changes
    ".config/emacs/binds.el".source = ./binds.el;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    # if user == "flakes"
    # then pkgs.emacs-pgtk
    # else pkgs.emacs;
    extraPackages = epkgs: [
      epkgs.magit
      epkgs.lsp-mode
      epkgs.nix-mode
      epkgs.rust-mode
      epkgs.python-mode
      epkgs.editorconfig
      epkgs.eglot
      epkgs.meow
      epkgs.devdocs
      epkgs.use-package
      epkgs.fzf
      epkgs.rg
      epkgs.rainbow-delimiters
      epkgs.base16-theme
    ];
    extraConfig = ''
      (require 'package)
      (require 'fzf)
      (add-to-list 'custom-theme-load-path (expand-file-name "themes/" user-emacs-directory))

      ;; Load the current theme specified by the theme-switcher script
      (let ((theme-file (expand-file-name "themes/current-theme.el" user-emacs-directory)))
      (when (file-exists-p theme-file)
        (load-file theme-file)))

      (let ((bindings-file (expand-file-name "binds.el" user-emacs-directory)))
      (when (file-exists-p bindings-file)
        (load-file bindings-file)))

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
