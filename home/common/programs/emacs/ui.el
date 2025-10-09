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

                                        ; (setq use-default-font-for-symbols nil)
                                        ; (set-fontset-font t 'unicode "Noto Emoji" nil 'append)

(set-face-background 'mode-line "gray13")

(defun ntf/mode-line-format (left right)
  (let ((available-width (- (window-width) (length left) 1)))
    (format (format "%%s %%%ds " available-width) left right)))

(defun ntf/get-base16-color (color-key)
  (let ((theme-colors
         (and
          (boundp (intern (format "base16-%s-theme-colors" 
                                  (replace-regexp-in-string "base16-\\(.*\\)" "\\1" 
                                                            (symbol-name (car custom-enabled-themes))))))
          (symbol-value (intern (format "base16-%s-theme-colors" 
                                        (replace-regexp-in-string "base16-\\(.*\\)" "\\1" 
                                                                  (symbol-name (car custom-enabled-themes)))))))))
    (when theme-colors
      (plist-get theme-colors color-key))))

(defun ntf/get-meow-indicator-colors (indicator)
  (let ((base00 (ntf/get-base16-color :base00))
        (base05 (ntf/get-base16-color :base05))
        (base07 (ntf/get-base16-color :base07))
        (base08 (ntf/get-base16-color :base08))
        (base09 (ntf/get-base16-color :base09))
        (base0B (ntf/get-base16-color :base0B))
        (base0D (ntf/get-base16-color :base0D))
        (base0E (ntf/get-base16-color :base0E)))
    (cond
     ((string= indicator "I")  ; Insert mode
      (list (or base05 "#a6e3a1") (or base00 "black")))
     ((string= indicator "N")  ; Normal mode  
      (list (or base08 "#cba6f7") (or base00 "black")))
     ((string= indicator "B")  ; Beacon mode
      (list (or base09 "#74c7ec") (or base00 "black")))
     ((string= indicator "M")  ; Motion mode
      (list (or base08 "#f38ba8") (or base00 "black")))
     (t  ; Fallback
      (list (or base0E "#cba6f7") (or base00 "black"))))))

;; Set the global mode-line format
(setq-default mode-line-format
              '((:eval (ntf/mode-line-format
                        (format-mode-line
                         '(" "
                           (:eval
                            (let* ((ind (substring (meow-indicator) 1 2))
                                   (colors (ntf/get-meow-indicator-colors ind)))
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
                           (:eval (when (and (featurep 'org-clock)
                                             (fboundp 'org-clock-is-active)
                                             (org-clock-is-active))
                                    (org-clock-get-clock-string)))
                           " "
                           ;; Version control info
                           (vc-mode vc-mode)))))))

(setq display-time-string-forms
      '((propertize (format-time-string "%H:%M") 'face 'bold)))
(display-time-mode 1)

(message "---> ui.el loaded successfully!")
