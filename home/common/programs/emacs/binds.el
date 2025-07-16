;; semilin mnx emacs meow
(use-package meow
  :ensure t
  :init
  (defconst meow-cheatsheet-layout-rain
    '((<TLDE> "`" "~")
      (<AE01> "1" "!")
      (<AE02> "2" "@")
      (<AE03> "3" "#")
      (<AE04> "4" "$")
      (<AE05> "5" "%")
      (<AE06> "6" "^")
      (<AE07> "7" "&")
      (<AE08> "8" "*")
      (<AE09> "9" "(")
      (<AE10> "0" ")")
      (<AE11> "-" "_")
      (<AE12> "=" "+")
      ;; Top row
      (<AD01> "," "<")   ; f -> ,
      (<AD02> "f" "F")   ; l -> f
      (<AD03> "l" "L")   ; h -> l
      (<AD04> "k" "K")   ; v -> k
      (<AD05> "v" "V")   ; z -> v
      (<AD06> "~" "^")   ; ' -> ~
      (<AD07> "w" "W")   ; w -> w
      (<AD08> "o" "O")   ; u -> o
      (<AD09> "u" "U")   ; o -> u
      (<AD10> "." ">")   ; y -> .
      (<AD11> "[" "{")
      (<AD12> "]" "}")
      (<BKSL> "\\" "|")
      ;; Home row
      (<AC01> "b" "B")   ; s -> b (This is your Caps Lock position)
      (<AC02> "s" "S")   ; r -> s
      (<AC03> "h" "H")   ; n -> h
      (<AC04> "t" "T")   ; t -> t
      (<AC05> "m" "M")   ; k -> m
      (<AC06> "c" "C")   ; c -> c
      (<AC07> "d" "D")   ; d -> d
      (<AC08> "a" "A")   ; e -> a
      (<AC09> "e" "E")   ; a -> e
      (<AC10> "i" "I")   ; i -> i
      (<AC11> "'" "\"")  ; ; -> '
      ;; Bottom row
      (<AB01> "p" "P")   ; x -> p
      (<AB02> "x" "X")   ; j -> x
      (<AB03> "j" "J")   ; b -> j
      (<AB04> "y" "Y")   ; m -> y
      (<AB05> "q" "Q")   ; q -> q
      (<AB06> "/" "?")   ; p -> /
      (<AB07> "g" "G")   ; g -> g
      (<AB08> ";" ":")   ; , -> ;
      (<AB09> "_" "_")   ; . -> _
      (<AB10> "z" "Z")   ; / -> z
      (<LSGT> "-" "_")))

  (defun meow-setup ()
    ;; --- Use the new RAIN cheatsheet ---
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-rain)
    (meow-motion-overwrite-define-key
     '("<escape>" . ignore))
    (meow-leader-define-key
     '("?" . meow-cheatsheet)
     ;; The leader key for "H-e" (original e) is now "a"
     '("a" . "H-e")
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument))
    ;; --- All keys remapped from Semimak-JQ to RAIN physical positions ---
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("1" . meow-expand-1)
     '("2" . meow-expand-2)
     '("3" . meow-expand-3)
     '("4" . meow-expand-4)
     '("5" . meow-expand-5)
     '("6" . meow-expand-6)
     '("7" . meow-expand-7)
     '("8" . meow-expand-8)
     '("9" . meow-expand-9)
     '("-" . negative-argument)
     '("'" . meow-reverse)                ; ; -> '
     '(";" . meow-inner-of-thing)         ; , -> ;
     '("_" . meow-bounds-of-thing)        ; . -> _
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("z" . meow-visit)                  ; / -> z
     '("e" . meow-right)                  ; a -> e
     '("E" . meow-right-expand)           ; A -> E
     '("j" . meow-back-word)              ; b -> j
     '("J" . meow-back-symbol)            ; B -> J
     '("c" . meow-left)                   ; c -> c (unchanged)
     '("C" . meow-left-expand)            ; C -> C (unchanged)
     '("d" . meow-next)                   ; d -> d (unchanged)
     '("D" . meow-next-expand)            ; D -> D (unchanged)
     '("a" . meow-prev)                   ; e -> a
     '("A" . meow-prev-expand)            ; E -> A
     '("f" . meow-find)                   ; l -> f (original find was on l)
     '("g" . meow-cancel-selection)       ; g -> g (unchanged)
     '("G" . meow-grab)                   ; G -> G (unchanged)
     '("i" . meow-append)                 ; i -> i (unchanged)
     '("I" . meow-open-below)             ; I -> I (unchanged)
     '("x" . meow-join)                   ; j -> x
     '("m" . meow-kill)                   ; k -> m
     '("M" . meow-delete)                 ; K -> M
     '("l" . meow-line)                   ; h -> l (original line was on h)
     '("L" . meow-goto-line)              ; H -> L
     '("y" . meow-mark-word)              ; m -> y
     '("Y" . meow-mark-symbol)            ; M -> Y
     '("h" . meow-change)                 ; n -> h
     '("u" . meow-block)                  ; o -> u
     '("U" . meow-to-block)               ; O -> U
     '("/" . meow-yank)                   ; p -> /
     '("q" . meow-quit)                   ; q -> q (unchanged)
     '("s" . meow-replace)                ; r -> s
     '("b" . meow-insert)                 ; s -> b
     '("B" . meow-open-above)             ; S -> B
     '("t" . meow-till)                   ; t -> t (unchanged)
     '("o" . meow-undo)                   ; u -> o
     '("O" . meow-undo-in-selection)      ; U -> O
     '("k" . meow-search)                 ; v -> k
     '("w" . meow-next-word)              ; w -> w (unchanged)
     '("W" . meow-next-symbol)            ; W -> W (unchanged)
     '("p" . meow-delete)                 ; x -> p
     '("P" . meow-backward-delete)        ; X -> P
     '("." . meow-save)                   ; y -> .
     '("v" . meow-pop-selection)          ; z -> v
     '("~" . repeat)                      ; ' -> ~
     '("(" . puni-backward-sexp)
     '(")" . puni-forward-sexp)
     '("^ s" . puni-splice)
     '("^ r" . puni-raise)
     '("^ b" . puni-barf-forward)
     '("^ l" . puni-slurp-forward)
     '("<escape>" . ignore)))
  :config
  (meow-setup)
  (meow-global-mode))
