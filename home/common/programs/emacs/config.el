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

; (setq ido-enable-flex-matching t)
; (setq ido-everywhere t)
; (ido-mode 1)
; (setq ido-use-filename-at-point 'guess)

(use-package projectile :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-globally-ignored-directories '("dist"))
  (setq projectile-indexing-method 'hybrid))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1)
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
   (java-mode . lsp-deferred)
   (lsp-mode . lsp-inlay-hints-mode))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-inlay-hints-enable t)
  (setq lsp-headerline-breadcrumb-mode nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-signature-render-documentation t)
  (setq lsp-idle-delay 0.2))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-show-with 'childframe)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions t))

(use-package treesit-auto
  :ensure t
  :defer t
  :config
  (setq treesit-auto-install nil)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

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

; (use-package vertico
;   :ensure t
;   :config
;   (vertico-mode +1))

; (use-package vertico
;   :if (package-installed-p 'vertico)
;   :demand t
;   :config
;   (setopt vertico-cycle t)
;   (vertico-mode +1))

; (use-package vertico-posframe
;   :ensure t
;   :init
;   (setq vertico-posframe-parameters
;         '((left-fringe . 8)
;           (right-fringe . 8)))
;   :config
;   (vertico-posframe-mode +1))

; (use-package orderless
;   :ensure t
;   :init
;   (setq completion-styles '(orderless basic partial-completion)
;         completion-category-defaults nil
;         completion-category-overrides '((file (styles partial-completion)))))

	

; (use-package marginalia
;   :custom
;   (marginalia-max-relative-age 0)
;   (marginalia-align 'right)
;   :init
;   (marginalia-mode))

; (use-package embark
;   :ensure t
;   :bind (("C-." . embark-act)
;          ("M-." . embark-dwim)
;          ("C-h B" . embark-bindings))
;   :init
;   (setq prefix-help-command #'embark-prefix-help-command)
;   :config
;   (add-to-list 'display-buffer-alist
;                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;                  nil
;                  (window-parameters (mode-line-format . none)))))

; (use-package consult
;   :ensure t
;   :bind (("C-x b" . consult-buffer)
;          ("C-x p b" . consult-project-buffer)
;          ("M-y" . consult-yank-pop)
;          ("M-g g" . consult-goto-line)
;          ("M-g ," . consult-line)
;          ("M-g i" . consult-imenu)
;          ("M-g I" . consult-imenu-multi)
;          ("M-g m" . consult-mark)
;          ("M-g o" . consult-outline)
;          ("M-g e" . consult-compile-error)
;          ("M-s d" . consult-fd)
;          ("M-s c" . consult-locate)
;          ("M-s g" . consult-grep)
;          ("M-s G" . consult-git-grep)
;          ("M-s r" . consult-ripgrep)
;          ("M-s l" . consult-line)
;          ("M-s L" . consult-line-multi)
;          ("M-s k" . consult-keep-lines)
;          ("M-s u" . consult-focus-lines)
;          ("M-s e" . consult-isearch-history)
;          :map isearch-mode-map
;          ("M-e" . consult-isearch-history)
;          ("M-s e" . consult-isearch-history)
;          ("M-s l" . consult-line)
;          ("M-s L" . consult-line-multi)
;          :map minibuffer-local-map
;          ("M-s" . consult-history)
;          ("M-r" . consult-history))
;   :config
;   (setq consult-find-args '("-L" . "-type f"))
;   (setq consult-preview-key 'any)
;   (consult-customize
;    consult-ripgrep consult-git-grep consult-grep consult-line
;    :preview-key '(:debounce 0.2 any))
;   (consult-customize
;    consult-find consult-fd consult-locate
;    :preview-key '(:debounce 0.4 any)
;    :state (consult--file-preview))
  
;   (autoload 'projectile-project-root "projectile")
;   (setq consult-project-function (lambda (_) (projectile-project-root)))
  
;   (defun consult-projectile-find-file ()
;     (interactive)
;     (if (projectile-project-p)
;         (let ((default-directory (projectile-project-root)))
;           (consult-fd))
;       (consult-fd))))

; (use-package embark-consult
;   :ensure t
;   :hook (embark-collect-mode . consult-preview-at-point-mode))


; (use-package marginalia
;   :ensure t
;   :config
;   (marginalia-mode))

(use-package vertico
  :defer 1
  :bind (("C-c v" . vertico-repeat)
         (:map vertico-map
               ("<return>" . vertico-directory-enter)
               ("<backspace>" . vertico-directory-delete-char)
               ("M-<backspace>" . vertico-directory-delete-word)
               ("M-q" . vertico-multiform-flat)
               ("C-c C-c" . embark-act)
               (">" . embark-become)
               ("M-*" . embark-act-all)
               ("C-<tab>" . embark-act-with-completing-read)
               ("C-c C-o" . embark-export)))
  :hook (minibuffer-setup . vertico-repeat-save)
  :config
  (require 'embark)
  (vertico-mode t)
  (vertico-multiform-mode t) ; M-{B(uffer) F(lat) G(rid) R(everse) U(nobtrusive) V(ertical)}'
  (setq vertico-resize nil
        vertico-count 15)
  ;; (setq vertico-multiform-commands nil)
  ;; (setq vertico-multiform-categories
  ;;       '((consult-location buffer)
  ;;         (consult-grep buffer)))
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (if (bound-and-true-p savehist-loaded)
      (add-to-list 'savehist-additional-variables
                   'vertico-repeat-history)))

(use-package embark
  :bind (("M-o" . embark-act)
          ("C-M-o" . embark-act-noquit)
          (:map minibuffer-local-map
                ("C-," . embark-become)
                ("C-<tab>" . embark-act-with-completing-read)
                ("C-<return>" . embark-dwim-noquit)))
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  ; (set-popup-rule! "^\\*Embark Export:" :size 0.35 :ttl 0 :quit nil)
  (setq embark-help-key "?")
  (setq embark-quit-after-action
        '((consult-projectile-embark-action-remove . nil)
          (t . t)))
  ;; (defvar +embark-become-keymap (define-keymap))
  (defun embark-act-noquit ()
    "Run action but don't quit the minibuffer afterwards."
    (interactive)
    (let ((embark-quit-after-action nil))
      (embark-act)))
  (defun embark-dwim-noquit()
    (interactive)
    (let ((embark-quit-after-action nil))
      (embark-dwim)
      (embark--restart)))
  (defun embark-act-with-completing-read (&optional arg)
    (interactive "P")
    (let* ((embark-prompter 'embark-completing-read-prompter)
           (embark-indicators '(embark-minimal-indicator)))
      (embark-act arg)))
  (defvar +embark-buffer-keymap (define-keymap))
  (define-key +embark-buffer-keymap "p" #'consult-projectile)
  (define-key +embark-buffer-keymap "P" #'consult-projectile-switch-project)
  (define-key +embark-buffer-keymap "a" #'affe-find)
  (define-key +embark-buffer-keymap "A" #'affe-find-no-ignore)
  (define-key +embark-buffer-keymap "b" #'consult-buffer)
  (define-key +embark-buffer-keymap "B" #'consult-project-buffer)
  (define-key +embark-buffer-keymap "f" #'find-file)

  (define-key embark-file-map "R" #'open-with-dragger)
  (define-key embark-file-map "L" #'copy-file-link-for-org)
  (add-to-list 'embark-become-keymaps '+embark-buffer-keymap))

(use-package embark-consult
  :defer 1
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  :config (consult-preview-at-point-mode)) 

(setq read-file-name-function #'consult-find-file-with-preview)

(defun consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
  (interactive)
  (let ((default-directory (or dir default-directory))
        (minibuffer-completing-file-name t))
    (consult--read #'read-file-name-internal :state (consult--file-preview)
                   :prompt prompt
                   :initial initial
                   :require-match mustmatch
                   :predicate pred)))

(setq project-read-file-name-function #'consult-project-find-file-with-preview)

(defun consult-project-find-file-with-preview (prompt all-files &optional pred hist _mb)
  (let ((prompt (if (and all-files
                         (file-name-absolute-p (car all-files)))
                    prompt
                  ( concat prompt
                    ( format " in %s"
                      (consult--fast-abbreviate-file-name default-directory)))))
        (minibuffer-completing-file-name t))
    (consult--read (mapcar
                    (lambda (file)
                      (file-relative-name file))
                    all-files)
                   :state (consult--file-preview)
                   :prompt (concat prompt ": ")
                   :require-match t
                   :history hist
                   :category 'file
                   :predicate pred)))

(use-package consult-projectile)
(use-package consult
  :bind (("C-x b"   . consult-buffer)
         ("C-x B"   . consult-project-buffer)
         ("C-x r b" . consult-bookmark)
         ("M-s g"   . consult-ripgrep)
         ("M-s r"   . consult-isearch-history)
         ("M-g M-g" . consult-goto-line)
         ("M-y"     . consult-yank-pop)
         ("C-x c i" . consult-imenu)
         ("C-M-s"   . consult-line)
         ("C-M-S-s" . consult-line-multi)
         (:map minibuffer-local-map
               ("M-s" . consult-history)
               ("M-r" . consult-history)))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq consult-preview-key 'any)
  (consult-customize
    consult-find
    consult-fd
    :state (consult--file-preview))
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq consult-narrow-key "<")
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'multi-occur :override #'consult-multi-occur))


(use-package marginalia
  :defer 1
  :config
  (marginalia-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  :config
  (setq orderless-component-separator 'orderless-escapable-split-on-space)
  (defun orderless-flex-if-twiddle (pattern _index _total)
    (when (string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))

  (defun orderless-first-initialism (pattern index _total)
    (if (= index 0) 'orderless-initialism))

  (defun orderless-without-if-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

  (setq orderless-style-dispatchers '(orderless-flex-if-twiddle
                                      orderless-without-if-bang)))



(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package windmove
  :bind (("S-<right>" . windmove-right)
         ("S-<left>" . windmove-left)
         ("S-<up>" . windmove-up)
         ("S-<down>" . windmove-down)))

;; Terminal support with vterm
(use-package vterm
  :ensure t
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000)
  (setq vterm-shell "bash"))

;; Better dired with zoxide integration
(use-package dired
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (setq dired-listing-switches "-agho --group-directories-first")
  (setq dired-dwim-target t))


(use-package zig-mode
  :ensure t
  :defer t
  :mode "\\.zig\\'")

;; Auto-mode associations for proper LSP activation
(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))

(message "---> config.el loaded successfully!")
(provide 'config)
