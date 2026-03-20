;; :Packages
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
  :custom
  (indent-bars-prefer-character t) 
  :hook (prog-mode . indent-bars-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; :Frame

(defun my/setup-new-frame (frame)
  (with-selected-frame frame
    (let ((theme-name 'custom-nix)
          (theme-file (expand-file-name "themes/custom-nix-theme.el" user-emacs-directory)))
      (when (file-exists-p theme-file)
        (load-file theme-file)
        (enable-theme theme-name)))
    
    (set-frame-parameter frame 'background-mode 'dark)
    
    (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
    (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
    (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
    
    (when (fboundp 'spacious-padding-mode)
      (spacious-padding-mode 1))))

;; :Status Line

(defun ntf/get-custom-theme-color (color-key)
  "Retrieve a color from the custom-theme-colors plist."
  (when (boundp 'custom-theme-colors)
    (plist-get custom-theme-colors color-key)))

(defun ntf/get-meow-indicator-colors (indicator)
  "Map Meow indicators to Helix-style statusline colors."
  (let ((pri      (ntf/get-custom-theme-color :primary))
        (on-pri   (ntf/get-custom-theme-color :on-primary))
        (sec      (ntf/get-custom-theme-color :secondary))
        (on-sec   (ntf/get-custom-theme-color :on-secondary))
        (ter      (ntf/get-custom-theme-color :tertiary))
        (on-ter   (ntf/get-custom-theme-color :on-tertiary))
        (on-cur   (ntf/get-custom-theme-color :cursor-fg))
        (cur      (ntf/get-custom-theme-color :cursor-bg)))
    (cond
     ((string= indicator "N") ; Normal Mode
      (list (or pri "#83a598") (or on-pri "black")))
     ((string= indicator "I") ; Insert Mode
      (list (or ter "#b8bb26") (or on-ter "black")))
     ((string= indicator "B") ; Beacon Mode
      (list (or sec "#fabd2f") (or on-sec "black")))
     ((string= indicator "M") ; Motion
      (list (or cur "#928374") (or on-cur "black")))
     (t ; Fallback (Beacon, etc.)
      (list (or cur "#3c3836") (or on-cur "white"))))))

(defun ntf/mode-line-format (left right)
  (let* ((width (window-width))
         (left-len (length (format-mode-line left)))
         (right-len (length (format-mode-line right)))
         (available-width (max 1 (- width left-len right-len))))
    (format "%s%s%s" left (make-string available-width ?\s) right)))

(setq-default mode-line-format
  '((:eval (ntf/mode-line-format
            (format-mode-line
             '(" "
               (:eval
                (let* ((ind (substring (meow-indicator) 1 2))
                       (colors (ntf/get-meow-indicator-colors ind)))
                  (propertize (concat " " ind " ") 'face `(:background ,(car colors) :foreground ,(cadr colors) :weight bold))))
               " %* "
               (:eval (propertize "%b" 'face 'bold))
               " %m "))
            (format-mode-line
             '((:eval (when (and (featurep 'org-clock) (org-clock-is-active))
                        (concat (org-clock-get-clock-string) " ")))
               " %l:%c "
               (vc-mode vc-mode)
               " "))))))


;; :Fixes
(add-hook 'after-make-frame-functions #'my/setup-new-frame)

(when (and (not (daemonp)) (display-graphic-p))
  (my/setup-new-frame (selected-frame)))

(setq display-time-string-forms '((propertize (format-time-string "%H:%M") 'face 'bold)))
(display-time-mode 1)

(message "---> ui.el loaded successfully!")
