(require 'use-package)

(column-number-mode)
(global-display-line-numbers-mode t)
(setq scroll-margin 5)
(setq scroll-step 1)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-clients-initialization-options
        '((zls . (:enable_inlay_hints t
                   :enable_snippets t)))))

(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil
              tab-width 2)
(setq kill-do-not-save-duplicates t)

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  :init
  (global-corfu-mode))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package zig-mode
  :ensure t
  :mode "\\.zig\\'"
  :hook (zig-mode . lsp-deferred))

(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("zls"))
                    :major-modes '(zig-mode)
                    :server-id 'zls)))


(with-eval-after-load 'lsp-mode
 (lsp-register-client
  (make-lsp-client :new-connection (lsp-stdio-connection '("rust-analyzer"))
                   :major-modes '(rust-mode)
                   :server-id 'rust-analyzer)))

(use-package rust-mode
 :ensure t
 :mode "\\.rs\\'"
 :hook (rust-mode . lsp-deferred))

(message "---> config.el loaded successfully!")
(provide 'config)
