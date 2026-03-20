(defun my/try-reload-config ()
  (interactive)
  (let* ((emacs-dir (expand-file-name "~/Flake/home/common/programs/emacs"))
         (all-files (directory-files emacs-dir nil "\\.el$")))
    
    (when (fboundp 'meow-global-mode) (meow-global-mode -1))
    
    (setq prog-mode-hook nil
          org-mode-hook nil
          dired-mode-hook nil)

    (dolist (file all-files)
      (unless (member file '("init.el" "current-theme.el" "rain.el" "qwerty.el"))
        (let ((full-path (expand-file-name file emacs-dir)))
          (if (file-exists-p full-path)
              (load full-path nil t t)
            (message "Skipping missing file: %s" file)))))

    (when (fboundp 'meow-global-mode) (meow-global-mode 1))
    
    (message "Live Flake Config Nuke-and-Paved successfully!")))
