(require 'use-package)

(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(setq scroll-margin 5)
(setq scroll-step 1)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(add-hook 'prog-mode-hook (lambda ()
                            (electric-pair-mode)))

(use-package all-the-icons :ensure t)
(use-package all-the-icons-dired
  :ensure t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (setq all-the-icons-dired-monochrome nil))

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
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)

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
  (setq lsp-headerline-breadcrumb-enable nil)
                                        ; (setq lsp-eldoc-render-all t)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-signature-render-documentation t)
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

  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-code-actions t))

(use-package treesit-auto
  :ensure t
  :defer t
  :config
  (setq treesit-auto-install nil)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


                                        ; --- Company Mode (Completion Framework) ---
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
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.1))

(setq insert-directory-program "ls")

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(use-package vertico
  :ensure t
  :config (vertico-mode +1))

(use-package vertico-posframe
  :ensure t
  :init
  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))
  :config (vertico-posframe-mode +1))

(use-package orderless
  :ensure t
  :init (setq completion-styles '(orderless basic partial-completion)
              completion-category-defaults nil
              completion-category-overrides ''(file (styles partial-completion))))

(use-package marginalia
  :ensure t
  :config (marginalia-mode))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings))
  :init (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("C-x p b" . consult-project-buffer)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         ("M-g ," . consult-line)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-g m" . consult-mark)
         ("M-g o" . consult-outline)
         ("M-g e" . consult-compile-error)
         ("M-s d" . consult-fd)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history)))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package windmove
  :bind (("S-<right>" . windmove-right)
         ("S-<left>" . windmove-left)
         ("S-<up>" . windmove-up)
         ("S-<down>" . windmove-down)))

(message "---> config.el loaded successfully!")
(provide 'config)
