(use-package meow
  :ensure t
  :init

  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (setq meow-use-cursor-position-hack t)
    (setq meow-use-clipboard t)

    (meow-motion-define-key
     '("j" . meow-next)
     '("l" . meow-prev)
     '("<escape>" . ignore))
    (setq meow-selection-command-fallback
          '((meow-change . meow-change-char)
            (meow-kill . meow-delete)
            (meow-cancel-selection . ignore)
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
     ; '("a" . execute-extended-command)
     
     ;; Buffer management (Space b prefix)
     '("e" . (lambda () (interactive)
	       (if (project-current nil)
		   (call-interactively #'project-eshell)
		 (call-interactively #'eshell))))

     '("f" . (lambda () (interactive)
	       (if (project-current nil)
		   (call-interactively #'project-find-file)
		 (call-interactively #'find-file))))
     '("F" . find-file)

     '("d" . (lambda () (interactive)
	       (if (project-current nil)
		   (call-interactively #'project-dired)
		 (call-interactively #'dired))))
     '("E" . dired)

     '("b" . (lambda ()
	       (interactive)
	       (if (eq major-mode 'erc-mode)
		   (call-interactively #'erc-switch-to-buffer)
		 (if (project-current nil)
		     (call-interactively #'project-switch-to-buffer)
		   (call-interactively #'switch-to-buffer)))))
     '("B" . switch-to-buffer)

     '("k" . kill-buffer)
     '("K" . project-kill-buffers)
     '("p" . project-switch-project)

     '("o" . other-window)
     '("O" . delete-window)
     '("A" . previous-buffer)
     '("D" . next-buffer)
     '("t" . vterm-other-window)
     '("C" . (lambda ()
           (interactive)
           (if (project-current nil)
               (call-interactively #'project-compile)
             (call-interactively #'compile))))
     ; '("C" . compile)
     '("i" . imenu)
     '("n" . meow-leader-prefix)
     '("/" . meow-keypad-describe-key)
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
     '("-" . negative-argument)
     
     ; movement
     '("i" . meow-prev)
     '("k" . meow-next)
     '("j" . meow-left)
     '("l" . meow-right)

     '("y" . meow-search)
     '("/" . meow-visit)

     ; expansion
     '("I" . meow-prev-expand)
     '("K" . meow-next-expand)
     '("J" . meow-left-expand)
     '("L" . meow-right-expand)

     '("u" . meow-back-word)
     '("U" . meow-back-symbol)
     '("o" . meow-next-word)
     '("O" . meow-next-symbol)

     '("a" . meow-mark-word)
     '("A" . meow-mark-symbol)
     '("s" . meow-line)
     '("w" . meow-block)
     '("q" . meow-join)
     '("g" . meow-grab)
     '("G" . meow-pop-grab)
     '("p" . meow-cancel-selection)
     '("P" . meow-pop-selection)

     '("t" . meow-till)
     '("T" . meow-find)

     '("," . meow-beginning-of-thing)
     '("." . meow-end-of-thing)
     '("<" . meow-inner-of-thing)
     '(">" . meow-bounds-of-thing)

     '("[" . indent-rigidly-left-to-tab-stop)
     '("]" . indent-rigidly-right-to-tab-stop)

     ; editing
     '("b" . open-line)
     '("B" . split-line)
     '("d" . meow-kill)
     '("f" . meow-change)
     '("F" . meow-change-line)
     '("x" . meow-delete)
     '("c" . meow-save)
     '("C" . meow-save-clipboard)
     '("v" . meow-yank-dwim)
     '("V" . meow-yank-pop-dwim)

     '("e" . meow-insert)
     '("E" . meow-open-above)
     '("r" . meow-append)
     '("R" . meow-open-below)

     '("z" . query-replace-regexp)

     '("h" . undo-only)
     '("H" . undo-redo)

     '("m" . meow-kmacro)
     '("M" . kmacro-call-macro)

     ; prefix n
     '(";m" . kmacro-edit-macro)
     '(";c" . meow-comment)

     ; prefix ;
     '(";f" . save-buffer)
     '(";F" . save-some-buffers)

     '("<escape>" . ignore)))

  :config
  (meow-setup)
  (meow-global-mode))

(defun my/dired-setup ()
  (define-key dired-mode-map (kbd "z") 'my/dired-zoxide-jump))

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

(defun meow-kmacro ()
  "Toggle recording of kmacro."
  (interactive)
  (if defining-kbd-macro
      (meow-end-kmacro)
    (meow-start-kmacro)))

;; -------------------- ;;
;;       VARIABLES      ;;
;; -------------------- ;;
(meow-thing-register 'angle
                     '(pair ("<") (">"))
                     '(pair ("<") (">")))

(setq meow-char-thing-table
      '((?f . round)
        (?d . square)
        (?s . curly)
        (?a . angle)
        (?r . string)
        (?v . paragraph)
        (?c . line)
        (?x . buffer)))

(message "---> binds.el loaded successfully!")
