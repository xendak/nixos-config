(use-package meow
  :ensure t
  :init

  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (setq meow-use-cursor-position-hack t)
    (setq meow-use-clipboard t)

    (meow-motion-define-key
     '("s" . meow-next)
     '("w" . meow-prev)
     '("<escape>" . ignore))
    (setq meow-selection-command-fallback
          '((meow-change . meow-change-char)
            (meow-kill . meow-delete)
            (meow-replace . my-replace)
            (meow-cancel-selection . keyboard-quit)
            (meow-pop-selection . meow-pop-grab)
            (meow-beacon-change . meow-beacon-change-char)))
    (meow-leader-define-key
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     
     '("SPC" . meow-M-x)
     '("<SPC>" . meow-M-x)

     '("o" . my-smart-eshell)
     '("d" . my-smart-find-file)
     '(";" . my-smart-compile)
     '("e" . my-smart-dired)
     '("a" . my-compile-and-run)
     '("," . my-smart-switch-buffer)

     '("D" . find-file)
     '("O" . dired)
     '("h" . kill-buffer)
     '("H" . project-kill-buffers)
     '("b" . project-switch-project)
     '("f" . other-window)
     '("F" . delete-window)
     '("p" . previous-buffer)
     '("n" . next-buffer)
     '("`" . vterm-other-window)
     '("l" . imenu)
     '("P" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))

    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     ; '("," . meow-reverse)
     ; '("'" . negative-argument)
     
     ; movement
     '("w" . meow-prev)
     '("s" . meow-next)
     '("a" . meow-left)
     '("d" . meow-right)

     '("t" . meow-search)
     '("/" . meow-visit)

     ; expansion
     '("W" . meow-prev-expand)
     '("S" . meow-next-expand)
     '("A" . meow-left-expand)
     '("D" . meow-right-expand)

     '("q" . meow-reverse)
     '("f" . meow-back-word)
     '("F" . meow-back-symbol)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)

     '(";" . meow-mark-word)
     '(":" . meow-mark-symbol)
     '("l" . meow-line)
     '("L" . meow-goto-line)
     '("z" . meow-block)
     '("p" . meow-join)
     '("h" . meow-grab)
     '("H" . meow-pop-grab)

     ; TODO:
     '("b" . meow-cancel-selection)
     '("B" . meow-pop-selection)

     '("." . meow-till)
     '("," . meow-find)

     '("c" . meow-beginning-of-thing)
     '("v" . meow-end-of-thing)
     '("C" . meow-inner-of-thing)
     '("V" . meow-bounds-of-thing)

     ; editing
     ; '("q" . open-line)
     ; '("Q" . split-line)

     '("j" . meow-backward-delete)
     '("J" . meow-kill-to-eol)
     '("k" . meow-change)
     '("K" . meow-change-line)
     '("m" . meow-kill)
     '("n" . meow-save)
     '("N" . meow-save-clipboard)
     '("g" . meow-yank)
     '("G" . meow-yank-pop)

     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("o" . meow-append)
     '("O" . meow-open-below)

     '("?" . query-replace-regexp)

     '("u" . undo-only)
     '("U" . undo-redo)

     '("r" . meow-replace)

     '("y" . meow-kmacro)
     '("Y" . kmacro-call-macro)

     ; hard paragraph movement
     '("[" . backward-paragraph)
     '("]" . forward-paragraph)
     '("{" . meow-backward-paragraph-expand)
     '("}" . meow-forward-paragraph-expand)
     '("<" . indent-rigidly-left-to-tab-stop)
     '(">" . indent-rigidly-right-to-tab-stop)


     ; prefixed keys?
     '("'" . my-prefix-key)
     '("<escape>" . ignore)))

  :config
  (meow-setup)
  (meow-global-mode))

(defvar my-prefix-key
  (let ((keymap (make-keymap)))
	(define-key keymap "d" #'next-buffer)
	(define-key keymap "a" #'previous-buffer)
    (define-key keymap "q" #'kmacro-edit-macro)
	(define-key keymap "w" #'kill-current-buffer)
    (define-key keymap "W" #'delete-window)
    (define-key keymap "," #'meow-reverse)
    (define-key keymap "'" #'negative-argument)
    (define-key keymap "u" #'meow-undo-in-selection)
    (define-key keymap "c" #'meow-comment)
    (define-key keymap "s" #'save-buffer)
    (define-key keymap "<SPC> s" #'save-some-buffers)
    (define-key keymap "<SPC> q" #'save-buffers-kill-terminal)
    keymap))
(defalias 'my-prefix-key my-prefix-key)

(defvar my-compile-and-run
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "c" #'compile)
    (define-key keymap "r" #'my-run-program)
    (define-key keymap "e" #'my-eshell-run-program)
    (define-key keymap "i" #'my-vterm-run-program)
    (define-key keymap "a" #'previous-error)
    (define-key keymap "d" #'next-error)
    keymap))

(meow-thing-register 'angle
                     '(pair ("<") (">"))
                     '(pair ("<") (">")))

(setq meow-char-thing-table
      '((?j . round)
        (?k . square)
        (?l . line)
        (?\; . angle)
        (?' . defun)
        (?m . string)
        (?, . paragraph)
        (?. . curly)
        (?/ . buffer)))

(message "---> binds.el loaded successfully!")
