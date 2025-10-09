{ config, pkgs, ... }:
let
  emacs_path = "/home/${config.home.username}/Flake/home/common/programs/emacs";

  baseFiles = [
    "config.el"
    "ui.el"
    "org.el"
  ];

  keyboardLayout = if config.home.username == "drops" then "qwerty.el" else "rain.el";

  startupFiles = baseFiles ++ [ keyboardLayout ];

  allLoadableFiles = baseFiles ++ [
    "qwerty.el"
    "rain.el"
  ];

  toElispList = files: "'(" + pkgs.lib.concatStringsSep " " (map (f: ''"${f}"'') files) + ")";

in
{
  home.file = {
    ".config/emacs/init.el".text = ''
      (require 'package)
      (setq inhibit-startup-screen t initial-scratch-message nil)
      (add-to-list 'default-frame-alist '(font . "${config.fontProfiles.monospace.family}-14"))

      (setq use-y-or-n-p t)
      (setq use-dialog-box nil)
      (setq package-check-signature nil)
      (setq read-buffer-completion-ignore-case t
            read-file-name-completion-ignore-case t
            completion-ignore-case t)

      (let ((backup-dir (expand-file-name "backups/" user-emacs-directory))
            (auto-saves-dir (expand-file-name "auto-saves/" user-emacs-directory)))
        (make-directory backup-dir t)
        (make-directory auto-saves-dir t)
        (setq backup-directory-alist `(("." . ,backup-dir))
              auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
              auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
              tramp-backup-directory-alist `((".*" . ,backup-dir))
              tramp-auto-save-directory auto-saves-dir))
      (setq backup-by-copying t
            delete-old-versions t
            version-control t
            kept-new-versions 2
            kept-old-versions 1)
      (setq auto-save-default nil)
      (add-to-list 'custom-theme-load-path (expand-file-name "themes/" user-emacs-directory))

      (defun my/internal-load-file (file)
        "Load a file from the flake."
        (let ((f (expand-file-name file "${emacs_path}")))
          (if (file-exists-p f)
              (load-file f)
            (warn "Could not find flake config file: %s" f))))

      (dolist (file ${toElispList startupFiles})
        (my/internal-load-file file))

      (defun my/load-from-flake (file)
        "Load a specific .el file from the flake with completion."
        (interactive
         (list (completing-read "Load from flake: " ${toElispList allLoadableFiles})))
        (my/internal-load-file file)
        (message "Successfully loaded %s" file))

      (defun my/reload-config ()
        "Reload Emacs config from flake."
        (interactive)
        (dolist (file ${toElispList startupFiles})
          (my/internal-load-file file))
        (message "Successfully reloaded core flake configuration."))
    '';
  };
}
