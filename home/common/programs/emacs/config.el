(require 'use-package)

(column-number-mode)
(global-display-line-numbers-mode t)
(setq scroll-margin 5)
(setq scroll-step 1)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package all-the-icons :ensure t)
(use-package all-the-icons-dired :ensure t)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-use-filename-at-point 'guess)

(use-package projectile :ensure t
   :config
   (projectile-global-mode)
   (setq projectile-globally-ignored-directories '("dist"))
   (setq projectile-indexing-method 'hybrid))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq tab-always-indent 'complete)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook
  ((c-mode . lsp-deferred)
   (c++-mode . lsp-deferred)
   (rust-mode . lsp-deferred)
   (python-mode . lsp-deferred)
   (zig-mode . lsp-deferred)
   (js-mode . lsp-deferred)
   (java-mode . lsp-deferred))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-inlay-hints-enable t)
  (setq lsp-headerline-breadcrumb-mode nil)
  (setq lsp-signature-render-documentation t)
  (setq lsp-eldoc-render-all t)
  (setq lsp-idle-delay 0.2)
  (define-key lsp-mode-map (kbd "i") #'lsp-inlay-hint-mode))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  ;; Show documentation in a floating window (childframe) at the cursor
  (setq lsp-ui-doc-show-with 'childframe)
  (setq lsp-ui-doc-position 'at-point)

  ;; Enable other nice UI elements
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-code-actions t))

(use-package treesit-auto
  :ensure t
  :defer t
  :config
  ;; We disable auto-install because we handle grammars with NixOS
  (setq treesit-auto-install nil)
  :init
  ;; Automatically enable and configure treesit-mode for supported major modes
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;;; --- Company Mode (Completion Framework) ---
(use-package company
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("<return>" . company-complete-selection)
              ("RET" . company-complete-selection))
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.1))

  ; (with-eval-after-load 'company
  ;   (define-key company-mode-map (kbd "<tab>") #'company-complete)
  ;   (define-key company-mode-map (kbd "TAB") #'company-complete)
  ;   (define-key company-active-map (kbd "<tab>") #'company-select-next)
  ;   (define-key company-active-map (kbd "TAB") #'company-select-next)))

;; Fix for non-bash shells like Nushell
(setq insert-directory-program "ls")

;; Optional: Make company-mode's popup look tidier
(setq company-tooltip-align-annotations t)

(message "---> config.el loaded successfully!")
(provide 'config)
