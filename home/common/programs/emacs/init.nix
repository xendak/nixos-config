{ config, ... }:
{
  home.file = {
    ".config/emacs/init.el".text = ''
      (require 'package)
      (require 'fzf)

      (add-to-list 'custom-theme-load-path (expand-file-name "themes/" user-emacs-directory))

      ;; Load keybinds
      (let ((bindings-file (expand-file-name "binds.el" user-emacs-directory)))
        (when (file-exists-p bindings-file)
          (load-file bindings-file)))

      (setq default-frame-alist
            '((menu-bar-lines . 0)
              (tool-bar-lines . 0)
              (scroll-bar-width . 0)
              (vertical-scroll-bars . nil)
              (horizontal-scroll-bars . nil)))

      (setq initial-frame-alist default-frame-alist)

      ;; Function to disable UI elements
      (defun disable-ui-elements ()
        "Disable menu bar, tool bar, scroll bar, and tab bar."
        (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
        (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
        (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
        (when (fboundp 'tab-bar-mode) (tab-bar-mode -1)))

      ;; Disable immediately
      (disable-ui-elements)

      ;; Function to apply mode-line colors from current theme
      (defun my/apply-theme-modeline-colors ()
        (when (boundp 'base16-default-theme-colors)
          (let* ((colors base16-default-theme-colors)
                 (bg-color (plist-get colors :base02))
                 (fg-color (plist-get colors :base05))
                 (inactive-bg (plist-get colors :base01))
                 (inactive-fg (plist-get colors :base04)))
            
            (when (and bg-color fg-color)
              (set-face-attribute 'mode-line nil
                                  :background bg-color
                                  :foreground fg-color))
            
            (when (and inactive-bg inactive-fg)
              (set-face-attribute 'mode-line-inactive nil
                                  :background inactive-bg
                                  :foreground inactive-fg)))))

      ;; Function to apply mode-line colors to a specific frame
      (defun my/apply-theme-modeline-colors-to-frame (frame)
        "Apply mode-line colors from the current theme to a specific frame."
        (when (boundp 'base16-default-theme-colors)
          (let* ((colors base16-default-theme-colors)
                 (bg-color (plist-get colors :base02))
                 (fg-color (plist-get colors :base05))
                 (inactive-bg (plist-get colors :base01))
                 (inactive-fg (plist-get colors :base04)))
            
            (when (and bg-color fg-color)
              (set-face-attribute 'mode-line frame
                                  :background bg-color
                                  :foreground fg-color))
            
            (when (and inactive-bg inactive-fg)
              (set-face-attribute 'mode-line-inactive frame
                                  :background inactive-bg
                                  :foreground inactive-fg)))))

      (defun my/setup-frame-visuals (frame)
        "Apply theme and font settings to a new frame."
        (with-selected-frame frame
          (let ((theme-file (expand-file-name "themes/current-theme.el" user-emacs-directory)))
            (when (file-exists-p theme-file)
              (load-file theme-file)))
          
          ;; Set default font for GUI frames only
          (when (display-graphic-p frame)
            (add-to-list 'default-frame-alist '(font . "${config.fontProfiles.monospace.family} 14")))
          
          ;; Apply theme-based mode-line colors
          (my/apply-theme-modeline-colors-to-frame frame)
          
          ;; Also disable UI elements for new frames
          (disable-ui-elements)
          
          (message "---> My init.el was loaded successfully!")))

      ;; Hook to apply mode-line colors after theme changes
      (add-hook 'after-load-theme-hook #'my/apply-theme-modeline-colors)

      ;; Also apply after theme loading
      (add-hook 'emacs-startup-hook 
                (lambda () 
                  (run-with-timer 0.1 nil #'my/apply-theme-modeline-colors)))

      ;; Hook for new frames created by daemon
      (add-hook 'after-make-frame-functions #'my/setup-frame-visuals)

      ;; Backup hook
      (add-hook 'window-setup-hook 'disable-ui-elements)

      (when (and (not (daemonp)) (display-graphic-p))
        (my/setup-frame-visuals (selected-frame)))

      ;; General Settings
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

      (use-package rainbow-delimiters
        :hook ((lisp-mode emacs-lisp-mode scheme-mode hy-mode) . rainbow-delimiters-mode))
    '';
  };
}
