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
            (meow-cancel-selection . keyboard-quit)
            (meow-pop-selection . meow-pop-grab)
            (meow-beacon-change . meow-beacon-change-char)))
    (meow-leader-define-key
     ;; Digit arguments
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
     
     ;; General commands
     '("SPC" . meow-M-x)
     '("<SPC>" . meow-M-x)
     ; '("I" . execute-extended-command)
     
     ;; FIGURE THIS OUT,TIS NOT GOOD ATM
     '("o" . (lambda () (interactive)
	       (if (project-current nil)
		   (call-interactively #'project-eshell)
		 (call-interactively #'eshell))))

     '("d" . (lambda () (interactive)
	       (if (project-current nil)
		   (call-interactively #'project-find-file)
		 (call-interactively #'find-file))))
     '("D" . find-file)

     '("a" . (lambda () (interactive)
	       (if (project-current nil)
		   (call-interactively #'project-dired)
		 (call-interactively #'dired))))
     '("O" . dired)

     '("," . (lambda ()
	       (interactive)
	       (if (eq major-mode 'erc-mode)
		   (call-interactively #'erc-switch-to-buffer)
		 (if (project-current nil)
		     (call-interactively #'project-switch-to-buffer)
		   (call-interactively #'switch-to-buffer)))))
     '("," . switch-to-buffer)

     '("h" . kill-buffer)
     '("H" . project-kill-buffers)
     '("b" . project-switch-project)

     '("n" . my-prefix-key)

     '("f" . other-window)
     '("F" . delete-window)
     '("I" . previous-buffer)
     '("A" . next-buffer)
     '("`" . vterm-other-window)
     '(";" . (lambda ()
           (interactive)
           (if (project-current nil)
               (call-interactively #'project-compile)
             (call-interactively #'compile))))
     ; '(";" . compile)
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

     '("q" . meow-back-word)
     '("Q" . meow-back-symbol)
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
     '("r" . meow-find)

     '("c" . meow-beginning-of-thing)
     '("v" . meow-end-of-thing)
     '("C" . meow-inner-of-thing)
     '("V" . meow-bounds-of-thing)

     '("{" . indent-rigidly-left-to-tab-stop)
     '("}" . indent-rigidly-right-to-tab-stop)

     ; editing
     ; '("q" . open-line)
     ; '("Q" . split-line)

     '("j" . meow-kill)
     '("k" . meow-change)
     '("K" . meow-change-line)
     '("m" . meow-delete)
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

     '("y" . meow-kmacro)
     '("Y" . kmacro-call-macro)

     ; hard paragraph movement
     '("[" . backward-paragraph)
     '("]" . forward-paragraph)

     ; prefixed keys?
     '("' r" . meow-replace)
     '("' ," . meow-reverse)
     '("' '" . negative-argument)
     '("' u" . meow-undo-in-selection)
     '("' c" . meow-comment)
     '("' W" . delete-window)
     '("' q" . kill-current-buffer)
     '("' s" . save-buffer)
     '("' <SPC> s" . save-some-buffers)
     '("' <SPC> q" . save-buffers-kill-terminal)

     '("<escape>" . ignore)))

  :config
  (meow-setup)
  (meow-global-mode))

(defun goto-match-paren (arg)
  "Go to the matching paren/bracket, similar to vi's %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))))

(defun my/dired-setup ()
  (define-key dired-mode-map (kbd "Z") 'my/dired-zoxide-jump))

(add-hook 'dired-mode-hook 'my/dired-setup)

(defun my/dired-zoxide-jump ()
  (interactive)
  (let* ((zoxide-output (shell-command-to-string "zoxide query -l"))
         (dirs (split-string zoxide-output "\n" t))
         (selected-dir (completing-read "Jump to: " dirs)))
    (when selected-dir
      (dired selected-dir))))

(defun vterm-other-window ()
  (interactive)
  (let ((buf (generate-new-buffer "*vterm*")))
    (switch-to-buffer-other-window buf)
    (vterm-mode)))

;; -------------------- ;;
;;         UTILS        ;;
;; -------------------- ;;

(defvar my-prefix-key
  (let ((keymap (make-keymap)))
	(define-key keymap "b" #'meow-undo-in-selection)
	(define-key keymap "u" #'next-buffer)
	(define-key keymap "o" #'previous-buffer)
    (define-key keymap "m" #'kmacro-edit-macro)
    (define-key keymap "y" #'meow-comment)
	(define-key keymap "q" #'kill-current-buffer)
	(define-key keymap "w" #'delete-window)
    keymap))
(defalias 'my-prefix-key my-prefix-key)
(global-set-key (kbd "C-c n") 'my-prefix-key)

(defun meow-change-line ()
  "Kill till end of line and switch to INSERT state."
  (interactive)
  (let ((beg (point)))
    (end-of-line)
    (delete-region beg (point))
    (meow-insert-mode)))

(defun meow-save-clipboard ()
  "Copy in clipboard."
  (interactive)
  (let ((meow-use-clipboard t))
    (meow-save)))

(defun meow-smart-reverse ()
  "Reverse selection or begin negative argument."
  (interactive)
  (if (use-region-p)
      (meow-reverse)
    (negative-argument nil)))

(defun meow-kmacro ()
  "Toggle recording of kmacro."
  (interactive)
  (if defining-kbd-macro
      (kmacro-end-macro)
    (kmacro-start-macro)))

(meow-thing-register 'angle
                     '(pair ("<") (">"))
                     '(pair ("<") (">")))

(setq meow-char-thing-table
      '((?j . round)
        (?k . square)
        (?l . curly)
        (?; . angle)
        (?' . defun)
        (?m . string)
        (?, . paragraph)
        (?. . line)
        (?/ . buffer)))

; if i dont know the command name.. this is useful
(let ((current-command (global-lookup-key (kbd "C-x C-c"))))
  (when current-command
    (global-set-key (kbd "C-x C-q") current-command)
    (global-unset-key (kbd "C-x C-c"))))

(message "---> binds.el loaded successfully!")
