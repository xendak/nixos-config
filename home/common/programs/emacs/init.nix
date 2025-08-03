{ config, ... }:
let
  emacs_path = "/home/${config.home.username}/Flake/home/common/programs/emacs";
in
{
  home.file = {
    ".config/emacs/init.el".text = ''
      (require 'package)
      (setq inhibit-startup-screen t initial-scratch-message nil)
      (add-to-list 'default-frame-alist '(font . "${config.fontProfiles.monospace.family}-14"))


      ;; General Settings
      (setq use-y-or-n-p t)
      (setq initial-scratch-message nil)
      (setq use-dialog-box nil)
      (setq inhibit-startup-screen t)
      (setq package-check-signature nil)
      (setq read-buffer-completion-ignore-case t
            read-file-name-completion-ignore-case t
            completion-ignore-case t)

      ;; Backup and Auto-Save Configuration
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

      (mapc 'load-file (directory-files "${emacs_path}" t "\\.el\\'"))

      (defun my/load-from-flake (file)
        "Load a file from the flake emacs directory."
        (interactive)
        (let ((f (expand-file-name file "${emacs_path}")))
          (if (file-exists-p f)
              (load-file f)
            (warn "Could not find flake config file: %s" f))))

      (defun my/reload-config ()
          "Reload all Emacs config files from the flake."
          (interactive)
          (mapc 'load-file (directory-files "${emacs_path}" t "\\.el\\'"))
          (message "Successfully reloaded flake configuration."))
    '';
  };
}
