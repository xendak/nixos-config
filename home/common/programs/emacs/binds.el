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
     '("h" . meow-next)
     '("l" . meow-prev)
     '("<escape>" . ignore))
    (setq meow-selection-command-fallback
          '((meow-change . meow-change-char)
            (meow-kill . meow-delete)
            (meow-cancel-selection . ignore)
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
     '("b" . buffer)
     '("f" . dired)
     '("a" . execute-extended-command)
     '("d" . compile)
     '("e" . eval-buffer)
     '("i" . imenu)
     '("l" . lsp-mode-map)
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
     
     '("K" . meow-mark-symbol)
     '("k" . meow-next-symbol)
     '("n" . meow-back-word)
     '("b" . meow-back-symbol)
     '("m" . meow-next-word)
     '("r" . meow-replace)
     '("/" . isearch-forward)

     '("s" . meow-left)
     '("h" . meow-next)
     '("l" . meow-prev)
     '("t" . meow-right)
     
     '("e" . meow-change)
     '("d" . meow-kill)
     '("a" . meow-append)
     '("i" . meow-insert)
     
     '("S" . meow-left-expand)
     '("H" . meow-next-expand)
     '("T" . meow-right-expand)
     '("L" . meow-prev-expand)
     
     '("u" . meow-undo)
     '("o" . meow-block)
     '("w" . meow-mark-word)
     '("y" . meow-save)
     '("p" . meow-yank)
     
     '("f" . meow-find)
     '("v" . meow-visit)
     '("_" . meow-line)
     
     '("x" . meow-cancel-selection)
     '("j" . meow-till)
     '("q" . meow-quit)
     '("z" . meow-grab)
     '("Z" . meow-swap-grab)

     '("g" . meow-inner-of-thing)
     '(";" . meow-bounds-of-thing)
     '("G" . meow-beginning-of-thing)
     '(":" . meow-end-of-thing)
     
     '("c" . meow-reverse)
     '("'" . repeat)
     
     '("A" . meow-open-below)
     '("D" . meow-backward-delete)
     '("E" . meow-next-symbol)
     '("I" . meow-open-above)
     '("O" . meow-to-block)
     '("Y" . meow-sync-grab)
     '("Q" . meow-undo-in-selection)
     '("U" . meow-goto-line)
     '("V" . meow-search)
     '("W" . meow-set-mark)
     '("X" . meow-line)
     '("P" . meow-pop-selection)
     
     '("<escape>" . ignore)))

  :config
  (meow-setup)
  (meow-global-mode))

(message "---> binds.el loaded successfully!")
