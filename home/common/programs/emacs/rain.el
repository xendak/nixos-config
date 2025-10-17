(use-package meow
  :ensure t
  :init

  (defconst meow-cheatsheet-physical-layout-split
    "
   ┏━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┓           ┏━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┓
   ┃         │  <AE02> │  <AE03> │  <AE04> │  <AE05> │  <AE06> ┃           ┃  <AE07> │  <AE08> │  <AE09> │  <AE10> │  <AE11> │         ┃
   ┃         │         │         │         │         │         ┃           ┃         │         │         │         │         │         ┃
   ┠─────────┼─────────┼─────────┼─────────┼─────────┼─────────┨           ┠─────────┼─────────┼─────────┼─────────┼─────────┼─────────┨
   ┃         │         │         │         │         │         ┃           ┃         │         │         │         │         │         ┃
   ┃         │         │         │         │         │         ┃           ┃         │         │         │         │         │         ┃
   ┠─────────┼─────────┼─────────┼─────────┼─────────┼─────────┨           ┠─────────┼─────────┼─────────┼─────────┼─────────┼─────────┨
   ┃         │  <AD02> │  <AD03> │  <AD04> │  <AD05> │  <AD06> ┃           ┃  <AD07> │  <AD08> │  <AD09> │  <AD10> │  <AD11> │         ┃
   ┃         │         │         │         │         │         ┃           ┃         │         │         │         │         │         ┃
   ┠─────────┼─────────┼─────────┼─────────┼─────────┼─────────┨           ┠─────────┼─────────┼─────────┼─────────┼─────────┼─────────┨
   ┃         │         │         │         │         │         ┃           ┃         │         │         │         │         │         ┃
   ┃         │         │         │         │         │         ┃           ┃         │         │         │         │         │         ┃
   ┠─────────┼─────────┼─────────┼─────────┼─────────┼─────────┨           ┠─────────┼─────────┼─────────┼─────────┼─────────┼─────────┨
   ┃  <AC01> │  <AC02> │  <AC03> │  <AC04> │  <AC05> │  <AC06> ┃           ┃  <AC07> │  <AC08> │  <AC09> │  <AC10> │  <AC11> │  <AE01> ┃
   ┃         │         │         │         │         │         ┃           ┃         │         │         │         │         │         ┃
   ┠─────────┼─────────┼─────────┼─────────┼─────────┼─────────┨           ┠─────────┼─────────┼─────────┼─────────┼─────────┼─────────┨
   ┃         │         │         │         │         │         ┃           ┃         │         │         │         │         │         ┃
   ┃         │         │         │         │         │         ┃           ┃         │         │         │         │         │         ┃
   ┠─────────┼─────────┼─────────┼─────────┼─────────┼─────────┨           ┠─────────┼─────────┼─────────┼─────────┼─────────┼─────────┨
   ┃         │  <AB02> │  <AB03> │  <AB04> │  <AB05> │  <AB06> ┃           ┃  <AB07> │  <AB08> │  <AB09> │  <AB10> │  <AD12> │         ┃
   ┃         │         │         │         │         │         ┃           ┃         │         │         │         │         │         ┃
   ┠─────────┼─────────┼─────────┼─────────┼─────────┼─────────┨           ┠─────────┼─────────┼─────────┼─────────┼─────────┼─────────┨
   ┃         │         │         │         │         │         ┃           ┃         │         │         │         │         │         ┃
   ┃         │         │         │         │         │         ┃           ┃         │         │         │         │         │         ┃
   ┗━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━┼─────────┼─────────┨           ┠─────────┼─────────┼━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━┛
                                           ┃  <BSKL> │         ┃           ┃         │  SPC    ┃
                                           ┃         │         ┃           ┃         │         ┃
                                           ┠─────────┼─────────┨           ┠─────────┼─────────┨
                                           ┃         │         ┃           ┃         │         ┃
                                           ┃         │         ┃           ┃         │         ┃
                                           ┗━━━━━━━━━┷━━━━━━━━━┛           ┗━━━━━━━━━┷━━━━━━━━━┛
")

  (defconst meow-cheatsheet-layout-rain
    '(;(<AE01> ""  "")
      (<AE02> "1"       "!")
      (<AE03> "2"       "@")
      (<AE04> "3"       "#")
      (<AE05> "4"       "$")
      (<AE06> "5"       "%")
      (<AE07> "6"       "^")
      (<AE08> "7"       "&")
      (<AE09> "8"       "*")
      (<AE10> "9"       "(")
      (<AE11> "0"       ")")
      (<AD02> "b"       "B")
      (<AD03> "f"       "F")
      (<AD04> "l"       "L")
      (<AD05> "k"       "K")
      (<AD06> "v"       "V")
      (<AD07> "~"       "`")
      (<AD08> "w"       "W")
      (<AD09> "o"       "O")
      (<AD10> "u"       "U")
      (<AD11> "."       ">")
      (<AC01> ","       "?")
      (<AC02> "n"       "N")
      (<AC03> "s"       "S")
      (<AC04> "h"       "H")
      (<AC05> "t"       "T")
      (<AC06> "m"       "M")
      (<AC07> "c"       "C")
      (<AC08> "d"       "D")
      (<AC09> "a"       "A")
      (<AC10> "e"       "E")
      (<AC11> "i"       "I")
      (<AE01> "'"       "\"")
      (<AB02> "p"       "P")
      (<AB03> "x"       "x")
      (<AB04> "j"       "J")
      (<AB05> "y"       "Y")
      (<AB06> "q"       "Q")
      (<AB07> "/"       "\\")
      (<AB08> "g"       "G")
      (<AB09> ";"       ":")
      (<AB10> "_"       "-")
      (<AD12> "z"       "Z")
      (<BSKL> "r"       "R")))

  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-rain)
    (setq meow-cheatsheet-physical-layout meow-cheatsheet-physical-layout-split)
    (setq meow-use-cursor-position-hack t)
    (setq meow-use-clipboard t)

    (meow-motion-define-key
     '("t" . meow-next)
     '("s" . meow-prev)
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
     ; '("I" . execute-extended-command)
     
     ;; Buffer management (Space b prefix)
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
     '("-" . negative-argument)
     
     ; movement
     '("l" . meow-prev)
     '("h" . meow-next)
     '("s" . meow-left)
     '("t" . meow-right)

     '("v" . meow-search)
     '("P" . meow-visit)

     ; expansion
     '("L" . meow-prev-expand)
     '("H" . meow-next-expand)
     '("S" . meow-left-expand)
     '("T" . meow-right-expand)

     '("f" . meow-back-word)
     '("F" . meow-back-symbol)
     '("k" . meow-next-word)
     '("K" . meow-next-symbol)

     '("i" . meow-mark-word)
     '("I" . meow-mark-symbol)
     '("e" . meow-line)
     '("u" . meow-block)
     '("." . meow-join)
     '("c" . meow-grab)
     '("C" . meow-pop-grab)
     '("b" . meow-cancel-selection)
     '("B" . meow-pop-selection)

     '("`" . meow-till)
     '("~" . meow-find)

     '("x" . meow-beginning-of-thing)
     '("j" . meow-end-of-thing)
     '("X" . meow-inner-of-thing)
     '("J" . meow-bounds-of-thing)

     '("[" . indent-rigidly-left-to-tab-stop)
     '("]" . indent-rigidly-right-to-tab-stop)

     ; edrting
     '("," . open-line)
     '("<" . split-line)
     '("a" . meow-kill)
     '("d" . meow-change)
     '("D" . meow-change-line)
     '("_" . meow-delete)
     '(";" . meow-save)
     '(":" . meow-save-clipboard)
     '("g" . meow-yank-dwim)
     '("G" . meow-yank-pop-dwim)

     '("w" . meow-insert)
     '("W" . meow-open-above)
     '("o" . meow-append)
     '("O" . meow-open-below)

     '("Z" . query-replace-regexp)

     '("m" . undo-only)
     '("M" . undo-redo)

     '("y" . meow-kmacro)
     '("Y" . kmacro-call-macro)

     ; prefix n

     ; prefix ;
     ; '("af" . save-buffer)
     ; '("aF" . save-some-buffers)

     '("<escape>" . ignore)))

  :config
  (meow-setup)
  (meow-global-mode))

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
	(define-key keymap "b" #'ivy-switch-buffer)
	(define-key keymap "n" #'next-buffer)
    (define-key keymap "m" #'kmacro-edit-macro)
    (define-key keymap "y" #'meow-comment)
	(define-key keymap "p" #'previous-buffer)
	(define-key keymap "q" #'kill-current-buffer)
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
      '((?d . round)
        (?a . square)
        (?e . curly)
        (?I . angle)
        (?w . string)
        (?g . paragraph)
        (?; . line)
        (?_ . buffer)))

(message "---> binds.el loaded successfully!")
