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
     '("d" . meow-next)
     '("e" . meow-prev)
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
     ; '("n" . execute-extended-command)
     
     '("l" . (lambda () (interactive)
	       (if (project-current nil)
		   (call-interactively #'project-eshell)
		 (call-interactively #'eshell))))

     '("t" . (lambda () (interactive)
	       (if (project-current nil)
		   (call-interactively #'project-find-file)
		 (call-interactively #'find-file))))
     '("T" . find-file)

     '("h" . (lambda () (interactive)
	       (if (project-current nil)
		   (call-interactively #'project-dired)
		 (call-interactively #'dired))))
     '("L" . dired)

     '("q" . (lambda ()
	       (interactive)
	       (if (eq major-mode 'erc-mode)
		   (call-interactively #'erc-switch-to-buffer)
		 (if (project-current nil)
		     (call-interactively #'project-switch-to-buffer)
		   (call-interactively #'switch-to-buffer)))))
     '("Q" . switch-to-buffer)

     '("a" . kill-buffer)
     '("A" . project-kill-buffers)
     '("." . project-switch-project)

     '("u" . other-window)
     '("U" . delete-window)
     '("N" . previous-buffer)
     '("H" . next-buffer)
     '("v" . vterm-other-window)
     '("J" . (lambda ()
           (interactive)
           (if (project-current nil)
               (call-interactively #'project-compile)
             (call-interactively #'compile))))
     ; '("J" . compile)
     '("o" . imenu)
     '("," . meow-leader-prefix)
     '("Z" . meow-keypad-describe-key)
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
     '("o" . meow-prev)
     '("a" . meow-next)
     '("d" . meow-left)
     '("e" . meow-right)

     '("`" . meow-search)
     '("Z" . meow-visit)

     ; expansion
     '("O" . meow-prev-expand)
     '("A" . meow-next-expand)
     '("D" . meow-left-expand)
     '("E" . meow-right-expand)

     '("w" . meow-back-word)
     '("W" . meow-back-symbol)
     '("u" . meow-next-word)
     '("U" . meow-next-symbol)

     '("n" . meow-mark-word)
     '("N" . meow-mark-symbol)
     '("s" . meow-line)
     '("f" . meow-block)
     '("b" . meow-join)
     '("m" . meow-grab)
     '("M" . meow-pop-grab)
     '("." . meow-cancel-selection)
     '("." . meow-pop-selection)

     '("v" . meow-till)
     '("V" . meow-find)

     '(";" . meow-beginning-of-thing)
     '("_" . meow-end-of-thing)
     '("<" . meow-inner-of-thing)
     '(">" . meow-bounds-of-thing)

     '("[" . indent-rigidly-left-to-tab-stop)
     '("]" . indent-rigidly-right-to-tab-stop)

     ; editing
     '("q" . open-line)
     '("Q" . split-line)
     '("h" . meow-kill)
     '("t" . meow-change)
     '("T" . meow-change-line)
     '("x" . meow-delete)
     '("j" . meow-save)
     '("J" . meow-save-clipboard)
     '("y" . meow-yank-dwim)
     '("Y" . meow-yank-pop-dwim)

     '("l" . meow-insert)
     '("L" . meow-open-above)
     '("k" . meow-append)
     '("K" . meow-open-below)

     '("p" . query-replace-regexp)

     '("c" . undo-only)
     '("C" . undo-redo)

     '("g" . meow-kmacro)
     '("G" . kmacro-call-macro)

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
  (define-key dired-mode-map (kbd "p") 'my/dired-zoxide-jump))

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
      '((?t . round)
        (?h . square)
        (?s . curly)
        (?n . angle)
        (?k . string)
        (?y . paragraph)
        (?j . line)
        (?x . buffer)))

(message "---> binds.el loaded successfully!")
