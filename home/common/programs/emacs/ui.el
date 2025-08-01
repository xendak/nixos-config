(use-package spacious-padding
  :ensure t
  :config
  (setq spacious-padding-widths
    '( :internal-border-width 20
       :header-line-width 4
       :mode-line-width 6
       :tab-width 4
       :right-divider-width 30
       :scroll-bar-width 8)))

(use-package indent-bars
  :ensure t
  :hook (prog-mode . indent-bars-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(defun my/setup-new-frame (frame)
  (with-selected-frame frame
    (let ((theme-file (expand-file-name "themes/current-theme.el" user-emacs-directory)))
      (when (file-exists-p theme-file)
        ; (disable-current-themes)
        (load-file theme-file)))

    (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
    (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
    (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
    (when (fboundp 'spacious-padding-mode-for-frame)
      (spacious-padding-mode 1)
      (spacious-padding-mode-for-frame))))
      

(defun my/apply-theme-modeline-colors ()
  (when (boundp 'base16-default-theme-colors)
    (let* ((colors base16-default-theme-colors)
           (bg-color (plist-get colors :base02))
           (fg-color (plist-get colors :base05))
           (inactive-bg (plist-get colors :base01))
           (inactive-fg (plist-get colors :base04)))
      (when (and bg-color fg-color)
        (set-face-attribute 'mode-line nil :background bg-color :foreground fg-color))
      (when (and inactive-bg inactive-fg)
        (set-face-attribute 'mode-line-inactive nil :background inactive-bg :foreground inactive-fg)))))



(add-hook 'after-make-frame-functions #'my/setup-new-frame)
(add-hook 'after-load-theme-hook #'my/apply-theme-modeline-colors)

(when (and (not (daemonp)) (display-graphic-p))
  (my/setup-new-frame (selected-frame)))

(spacious-padding-mode 1)

(setq use-default-font-for-symbols nil)
(set-fontset-font t 'unicode "Noto Emoji" nil 'append)

(set-face-background 'mode-line "gray13")

;; Helper function to create a mode-line that spans the window width
(defun ntf/mode-line-format (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width (- (window-width) (length left) 1)))
    (format (format "%%s %%%ds " available-width) left right)))

;; Set the global mode-line format
(setq-default mode-line-format
  '((:eval (ntf/mode-line-format
            ;; --- Left side of the mode-line ---
            (format-mode-line
             '(" "
               (:eval
                (let* ((ind (substring (meow-indicator) 1 2))
                       (colors (if (string= ind "I")
                                   '("#a6e3a1" "black") ; Green for Insert
                                 '("#cba6f7" "black")))) ; Purple for Normal
                  (propertize (concat " " ind " ") 'face `(:background ,(car colors) :foreground ,(cadr colors)))))
               " %* " ; Buffer modification status
               (:eval (propertize "%b" 'face 'bold)) ; Buffer name (bold)
               " %m " ; Major mode
               "%l:%c " ; Line and column
               mode-line-percent-position
               "%%"))
            ;; --- Right side of the mode-line ---
            (format-mode-line
             '(" "
               ;; Org clock string, if active
               (:eval (when (org-clock-is-active)
                        (org-clock-get-clock-string)))
               " "
               ;; Version control info
               (vc-mode vc-mode)))))))

(setq display-time-string-forms
      '((propertize (format-time-string "%H:%M") 'face 'bold)))
(display-time-mode 1)

(message "---> ui.el loaded successfully!")
