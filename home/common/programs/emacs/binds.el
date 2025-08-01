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
    (<AE02> "1"	"!")
    (<AE03> "2"	"@")
    (<AE04> "3"	"#")
    (<AE05> "4"	"$")
    (<AE06> "5"	"%")
    (<AE07> "6"	"^")
    (<AE08> "7"	"&")
    (<AE09> "8"	"*")
    (<AE10> "9"	"(")
    (<AE11> "0"	")")
    ; (<AE12> ""  "")
    ; (<AD01> ""  "")
    (<AD02> "b"	"B")
    (<AD03> "f"	"F")
    (<AD04> "l"	"L")
    (<AD05> "k"	"K")
    (<AD06> "v"	"V")
    (<AD07> "~"	"`")
    (<AD08> "w"	"W")
    (<AD09> "o"	"O")
    (<AD10> "u"	"U")
    (<AD11> "."	">")
    ; (<AD12> ""  "")
    (<AC01> ","  "?")
    (<AC02> "n"	"N")
    (<AC03> "s"	"S")
    (<AC04> "h"	"H")
    (<AC05> "t"	"T")
    (<AC06> "m"	"M")
    (<AC07> "c"	"C")
    (<AC08> "d"	"D")
    (<AC09> "a"	"A")
    (<AC10> "e"	"E")
    (<AC11> "i"	"I")
    (<AE01> "'"  "\"")
    ; (<AB01> ""  "")
    (<AB02> "p"	"P")
    (<AB03> "x"	"x")
    (<AB04> "j"	"J")
    (<AB05> "y"	"Y")
    (<AB06> "q"	"Q")
    (<AB07> "/"	"\\")
    (<AB08> "g"	"G")
    (<AB09> ";"	":")
    (<AB10> "_"	"-")
    (<AD12> "z"	"Z")
    ; (<AB12> ""  "")
    (<BSKL> "r"  "R")))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-rain)
  (setq meow-cheatsheet-physical-layout meow-cheatsheet-physical-layout-split)
  (setq meow-use-cursor-position-hack t)
  (setq meow-use-clipboard t)
  (meow-motion-define-key
   ;; Movement keys: l=up, s=left, h=down, t=right
   '("h" . meow-next)     ; down (was j)
   '("l" . meow-prev)     ; up (was k)
   '("<escape>" . ignore))
  (setq meow-selection-command-fallback
   '((meow-change . meow-change-char)
     (meow-kill . meow-delete)
     (meow-cancel-selection . ignore)
     (meow-pop-selection . meow-pop-grab)
     (meow-beacon-change . meow-beacon-change-char)))
  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments.
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
   '("s" . save-buffer)           ;; save file
   '("f" . dired)                 ;; file manager
   '("a" . execute-extended-command) ;; M-x equivalent
   '("e" . eval-buffer)           ;; evaluate buffer
   '("l" . lsp-mode-map)
   '("i" . imenu)                 ;; navigation
   '("d" . compile)               ;; compile
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
   
   ; Free keys -> R
   '("K" . meow-mark-symbol)
   '("k" . meow-next-symbol)   ; mark symbol (was W, moved)
   '("n" . meow-back-word)        ; search (very common)
   '("b" . meow-back-symbol)     ; back word (top row)
   '("m" . meow-next-word)          ; join (keep original)
   '("r" . meow-replace)       ; replace (was r, moved)
   '("/" . meow-search)

   ;; Left home row - frequently used commands
   '("s" . meow-left)          ; left movement
   '("h" . meow-next)          ; down movement  
   '("l" . meow-prev)          ; down movement  
   '("t" . meow-right)         ; right movement
   
   ;; Right home row - frequently used commands  
   '("e" . meow-change)        ; change (very common)
   '("d" . meow-kill)        ; delete (very common)
   '("a" . meow-append)        ; append (very common)
   '("i" . meow-insert)        ; insert (very common)
   
   ;; Movement expansions (shifted versions)
   '("S" . meow-left-expand)   ; expand left
   '("H" . meow-next-expand)   ; expand down
   '("T" . meow-right-expand)  ; expand right
   '("L" . meow-prev-expand)   ; up movement
   
   ;; Other frequently used commands repositioned
   '("u" . meow-undo)          ; undo (top row, easy reach)
   '("o" . meow-block)         ; block selection (top row)
   '("w" . meow-mark-word)     ; mark word (top row)
   '("y" . meow-save)          ; save/yank (moved from qwerty y position)
   '("p" . meow-yank)          ; paste (moved to bottom row)
   
   ;; Less frequent commands moved to edges/bottom
   '("f" . meow-find)          ; find (top row)
   '("v" . meow-visit)         ; visit (top row)
   '("_" . meow-line)          ; line selection (was x)
   
   ;; Bottom row repositioning
   '("x" . meow-cancel-selection) ; cancel (was g, moved to bottom)
   '("j" . meow-till)          ; till (was t, moved to make room for movement)
   '("q" . meow-quit)          ; quit (bottom row)
   '("z" . meow-grab)          ; pop selection (bottom row)
   '("Z" . meow-swap-grab)     ; pop selection (bottom row)

   '("g" . meow-inner-of-thing)
   '(";" . meow-bounds-of-thing)
   '("G" . meow-beginning-of-thing)
   '(":" . meow-end-of-thing)
   
   ;; Special characters and remaining commands
   '("c" . meow-reverse)       ; reverse (keep in same relative position)
   '("'" . repeat)
   
   ;; Shifted/capital versions for remaining commands
   '("A" . meow-open-below)
   '("D" . meow-backward-delete)
   '("E" . meow-next-symbol)
   '("I" . meow-open-above)
   '("O" . meow-to-block)
   '("Y" . meow-sync-grab)     ; sync grab (was Y, moved)
   '("Q" . meow-undo-in-selection) ; undo in selection (was U, moved)
   '("U" . meow-goto-line)     ; goto line (alternative, was X)
   '("V" . meow-visit)         ; visit (duplicate for convenience)
   '("W" . meow-mark-word)     ; mark word (duplicate for convenience)
   '("X" . meow-line)          ; line (duplicate for convenience)
   '("P" . meow-pop-selection) ; pop (duplicate for convenience)
   
   '("<escape>" . ignore)))

  :config
  (meow-setup)
  (meow-global-mode))

(message "---> binds.el loaded successfully!")

