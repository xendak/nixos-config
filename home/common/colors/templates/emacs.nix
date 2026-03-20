{ paletteSet, ... }:
let
  p = paletteSet.palette;
  name = "nix";
in
{
  "emacs/themes/custom-${name}-theme.el" =
    # clisp
    ''
      ;;; custom-${name}-theme.el
      ;;; Base16 override for my material colors
      (deftheme custom-${name}
        "A nix Emacs theme.")

      (setq custom-theme-colors
        '(:bg0          "${p.bg}"
          :bg1          "${p.surface_container_low}"
          :bg2          "${p.surface_container}"
          :bg3          "${p.surface_container_high}"
          :bg4          "${p.surface_container_highest}"
          :fg0          "${p.fg}"
          :fg1          "${p.on_surface}"
          :fg2          "${p.on_surface_variant}"
          :primary      "${p.primary}"
          :on-primary   "${p.on_primary}"
          :pri-cont     "${p.primary_container}"
          :on-pri-cont  "${p.on_primary_container}"
          :secondary    "${p.secondary}"
          :on-secondary "${p.on_secondary}"
          :sec-cont     "${p.secondary_container}"
          :on-sec-cont  "${p.on_secondary_container}"
          :tertiary     "${p.tertiary}"
          :on-tertiary  "${p.on_tertiary}"
          :ter-cont     "${p.tertiary_container}"
          :on-ter-cont  "${p.on_tertiary_container}"
          :outline      "${p.outline}"
          :outline-var  "${p.outline_variant}"
          :inv-surf     "${p.inverse_surface}"
          :inv-on-surf  "${p.inverse_on_surface}"
          :cursor-bg    "${p.cursor_bg}"
          :cursor-fg    "${p.cursor_fg}"
          :region-bg    "${p.selection_bg}"
          :region-fg    "${p.selection_fg}"
          :search-bg    "${p.secondary}"
          :search-fg    "${p.on_secondary}"

          ;; ── Syntax: Semantic aliases ──────────────────────────────────
          :keywords     "${p.keywords}"
          :labels       "${p.labels}"
          :strings      "${p.strings}"
          :builtins     "${p.builtins}"
          :types        "${p.types}"
          :functions    "${p.functions}"
          :macros       "${p.macros}"
          :specials     "${p.specials}"
          :constants    "${p.constants}"
          :modules      "${p.modules}"
          :tags         "${p.tags}"
          :numeric      "${p.numeric}"
          :punctuation  "${p.punctuation}"
          :comments     "${p.comments}"
          :inlay        "${p.inlay}"

          ;; ── Diagnostics: ANSI semantic names ─────────────────────────
          :red          "${p.red}"
          :orange       "${p.orange}"
          :green        "${p.green}"
          :blue         "${p.blue}"
          :cyan         "${p.cyan}"))

      (let (;; UI
            (bg0         (plist-get custom-theme-colors :bg0))
            (bg1         (plist-get custom-theme-colors :bg1))
            (bg2         (plist-get custom-theme-colors :bg2))
            (bg3         (plist-get custom-theme-colors :bg3))
            (bg4         (plist-get custom-theme-colors :bg4))
            (fg0         (plist-get custom-theme-colors :fg0))
            (fg1         (plist-get custom-theme-colors :fg1))
            (fg2         (plist-get custom-theme-colors :fg2))
            (primary     (plist-get custom-theme-colors :primary))
            (on-primary  (plist-get custom-theme-colors :on-primary))
            (pri-cont    (plist-get custom-theme-colors :pri-cont))
            (on-pri-cont (plist-get custom-theme-colors :on-pri-cont))
            (secondary   (plist-get custom-theme-colors :secondary))
            (on-secondary (plist-get custom-theme-colors :on-secondary))
            (sec-cont    (plist-get custom-theme-colors :sec-cont))
            (on-sec-cont (plist-get custom-theme-colors :on-sec-cont))
            (tertiary    (plist-get custom-theme-colors :tertiary))
            (on-tertiary (plist-get custom-theme-colors :on-tertiary))
            (ter-cont    (plist-get custom-theme-colors :ter-cont))
            (on-ter-cont (plist-get custom-theme-colors :on-ter-cont))
            (outline     (plist-get custom-theme-colors :outline))
            (outline-var (plist-get custom-theme-colors :outline-var))
            (inv-surf    (plist-get custom-theme-colors :inv-surf))
            (inv-on-surf (plist-get custom-theme-colors :inv-on-surf))
            (cursor-bg   (plist-get custom-theme-colors :cursor-bg))
            (cursor-fg   (plist-get custom-theme-colors :cursor-fg))
            (region-bg   (plist-get custom-theme-colors :region-bg))
            (region-fg   (plist-get custom-theme-colors :region-fg))
            (search-bg   (plist-get custom-theme-colors :search-bg))
            (search-fg   (plist-get custom-theme-colors :search-fg))
            ;; Syntax
            (keywords    (plist-get custom-theme-colors :keywords))
            (labels      (plist-get custom-theme-colors :labels))
            (strings     (plist-get custom-theme-colors :strings))
            (builtins    (plist-get custom-theme-colors :builtins))
            (types       (plist-get custom-theme-colors :types))
            (functions   (plist-get custom-theme-colors :functions))
            (macros      (plist-get custom-theme-colors :macros))
            (specials    (plist-get custom-theme-colors :specials))
            (constants   (plist-get custom-theme-colors :constants))
            (modules     (plist-get custom-theme-colors :modules))
            (tags        (plist-get custom-theme-colors :tags))
            (numeric     (plist-get custom-theme-colors :numeric))
            (punctuation (plist-get custom-theme-colors :punctuation))
            (comments    (plist-get custom-theme-colors :comments))
            (inlay       (plist-get custom-theme-colors :inlay))
            ;; Diagnostics
            (red         (plist-get custom-theme-colors :red))
            (orange      (plist-get custom-theme-colors :orange))
            (green       (plist-get custom-theme-colors :green))
            (blue        (plist-get custom-theme-colors :blue))
            (cyan        (plist-get custom-theme-colors :cyan)))

        (custom-theme-set-faces
         'custom-${name}

         ;; ── Built-in ──────────────────────────────────────────────────────
         `(default        ((t (:foreground ,fg0 :background ,bg0))))
         `(cursor         ((t (:background ,cursor-bg :foreground ,cursor-fg))))
         `(fringe         ((t (:background ,bg1 :foreground ,outline))))
         `(border         ((t (:background ,outline-var))))
         `(vertical-border ((t (:foreground ,outline-var))))
         `(hl-line        ((t (:background ,bg1))))
         `(col-highlight  ((t (:background ,bg1))))
         `(highlight      ((t (:background ,bg2))))
         `(region         ((t (:background ,region-bg :distant-foreground ,region-fg))))
         `(secondary-selection ((t (:background ,bg2 :distant-foreground ,fg0))))
         `(trailing-whitespace ((t (:background ,red))))

         `(header-line    ((t (:foreground ,fg1 :background ,bg2 :inherit mode-line))))
         `(mode-line      ((t (:foreground ,fg0 :background ,bg3))))
         `(mode-line-buffer-id ((t (:foreground ,primary :weight bold))))
         `(mode-line-emphasis  ((t (:foreground ,secondary :slant italic))))
         `(mode-line-highlight ((t (:foreground ,tertiary :weight bold))))
         `(mode-line-inactive  ((t (:foreground ,outline :background ,bg1))))

         `(minibuffer-prompt   ((t (:foreground ,primary :weight bold))))
         `(link           ((t (:foreground ,secondary :underline t))))
         `(link-visited   ((t (:foreground ,on-sec-cont :underline t))))
         `(button         ((t (:inherit link))))
         `(widget-field   ((t (:background ,bg2 :box (:line-width 1 :color ,outline-var)))))
         `(widget-button  ((t (:underline t))))

         ;; ── Diagnostics: ANSI semantic ────────────────────────────────────
         `(error          ((t (:foreground ,red :weight bold))))
         `(warning        ((t (:foreground ,orange :weight bold))))
         `(success        ((t (:foreground ,green :weight bold))))
         `(shadow         ((t (:foreground ,outline))))

         ;; ── Line numbers ──────────────────────────────────────────────────
         `(line-number              ((t (:foreground ,outline :background ,bg1))))
         `(line-number-current-line ((t (:foreground ,on-pri-cont :background ,sec-cont :weight bold))))

         ;; ── Tabs ──────────────────────────────────────────────────────────
         `(tab-bar              ((t (:background ,bg1))))
         `(tab-bar-tab          ((t (:foreground ,on-pri-cont :background ,pri-cont :weight bold))))
         `(tab-bar-tab-inactive ((t (:foreground ,fg1 :background ,bg1))))
         `(tab-line             ((t (:background ,bg1))))
         `(tab-line-tab         ((t (:background ,bg2))))
         `(tab-line-tab-inactive ((t (:background ,bg1 :foreground ,fg1))))
         `(tab-line-tab-current  ((t (:foreground ,on-pri-cont :background ,pri-cont :weight bold))))
         `(tab-line-highlight    ((t (:foreground ,fg0 :background ,bg2))))

         ;; ── Font lock (syntax) ────────────────────────────────────────────
         `(font-lock-comment-face           ((t (:foreground ,comments :slant italic))))
         `(font-lock-comment-delimiter-face ((t (:foreground ,comments :slant italic))))
         `(font-lock-doc-face               ((t (:foreground ,inlay :slant italic))))
         `(font-lock-doc-string-face        ((t (:foreground ,inlay))))
         `(font-lock-string-face            ((t (:foreground ,strings))))
         `(font-lock-keyword-face           ((t (:foreground ,keywords :weight bold))))
         `(font-lock-builtin-face           ((t (:foreground ,builtins))))
         `(font-lock-preprocessor-face      ((t (:foreground ,macros))))
         `(font-lock-function-name-face     ((t (:foreground ,functions))))
         `(font-lock-variable-name-face     ((t (:foreground ,fg0))))
         `(font-lock-type-face              ((t (:foreground ,types))))
         `(font-lock-constant-face          ((t (:foreground ,constants))))
         `(font-lock-number-face            ((t (:foreground ,numeric))))
         `(font-lock-operator-face          ((t (:foreground ,punctuation))))
         `(font-lock-punctuation-face       ((t (:foreground ,punctuation))))
         `(font-lock-bracket-face           ((t (:foreground ,punctuation))))
         `(font-lock-delimiter-face         ((t (:foreground ,punctuation))))
         `(font-lock-regexp-grouping-backslash  ((t (:foreground ,specials))))
         `(font-lock-regexp-grouping-construct  ((t (:foreground ,specials :weight bold))))
         `(font-lock-negation-char-face         ((t (:foreground ,red))))
         `(font-lock-warning-face               ((t (:foreground ,red :weight bold))))

         ;; ── Search & match ────────────────────────────────────────────────
         `(match           ((t (:foreground ,on-pri-cont :background ,pri-cont))))
         `(isearch         ((t (:foreground ,search-fg :background ,search-bg :inverse-video t))))
         `(lazy-highlight  ((t (:foreground ,on-sec-cont :background ,sec-cont :inverse-video t))))
         `(isearch-lazy-highlight-face ((t (:inherit lazy-highlight))))
         `(isearch-fail    ((t (:background ,red :foreground ,bg0))))
         `(completions-common-part ((t (:foreground ,secondary :weight bold))))

         ;; ── Help ─────────────────────────────────────────────────────────
         `(help-key-binding ((t (:foreground ,fg0 :background ,bg2
                                 :box (:line-width 1 :color ,outline-var)
                                 :inherit fixed-pitch))))

         ;; ── Dired ─────────────────────────────────────────────────────────
         `(dired-directory ((t (:foreground ,modules :weight bold))))
         `(dired-symlink   ((t (:foreground ,specials))))

         ;; ── Compilation ───────────────────────────────────────────────────
         `(compilation-column-number   ((t (:foreground ,numeric))))
         `(compilation-line-number     ((t (:foreground ,numeric))))
         `(compilation-message-face    ((t (:foreground ,blue))))
         `(compilation-mode-line-exit  ((t (:foreground ,green))))
         `(compilation-mode-line-fail  ((t (:foreground ,red))))
         `(compilation-mode-line-run   ((t (:foreground ,blue))))

         ;; ── Diff ─────────────────────────────────────────────────────────
         `(diff-added      ((t (:foreground ,green :background ,bg1))))
         `(diff-changed    ((t (:foreground ,orange :background ,bg1))))
         `(diff-removed    ((t (:foreground ,red :background ,bg1))))
         `(diff-header     ((t (:background ,bg2))))
         `(diff-file-header ((t (:background ,bg3 :weight bold))))
         `(diff-hunk-header ((t (:foreground ,cyan :background ,bg2))))

         ;; ── Diff-hl ───────────────────────────────────────────────────────
         `(diff-hl-change  ((t (:foreground ,orange))))
         `(diff-hl-delete  ((t (:foreground ,red))))
         `(diff-hl-insert  ((t (:foreground ,green))))

         ;; ── Git gutter ────────────────────────────────────────────────────
         `(git-gutter:added    ((t (:foreground ,green))))
         `(git-gutter:deleted  ((t (:foreground ,red))))
         `(git-gutter:modified ((t (:foreground ,orange))))

         ;; ── Magit ────────────────────────────────────────────────────────
         `(magit-branch                   ((t (:foreground ,outline :weight bold))))
         `(magit-branch-current           ((t (:foreground ,primary :weight bold :box t))))
         `(magit-branch-local             ((t (:foreground ,secondary :weight bold))))
         `(magit-branch-remote            ((t (:foreground ,tertiary :weight bold))))
         `(magit-diff-context-highlight   ((t (:background ,bg1 :foreground ,fg0))))
         `(magit-diff-file-header         ((t (:background ,bg2 :foreground ,fg0))))
         `(magit-hash                     ((t (:foreground ,on-sec-cont))))
         `(magit-section-highlight        ((t (:background ,bg1))))

         ;; ── Flycheck / flymake ────────────────────────────────────────────
         `(flycheck-error   ((t (:underline (:style wave :color ,red)))))
         `(flycheck-warning ((t (:underline (:style wave :color ,orange)))))
         `(flycheck-info    ((t (:underline (:style wave :color ,blue)))))

         `(flymake-error    ((t (:underline (:style wave :color ,red)))))
         `(flymake-warning  ((t (:underline (:style wave :color ,orange)))))
         `(flymake-note     ((t (:underline (:style wave :color ,blue)))))

         ;; ── Company ──────────────────────────────────────────────────────
         `(company-tooltip                ((t (:foreground ,fg0 :background ,bg2))))
         `(company-scrollbar-bg           ((t (:background ,bg3))))
         `(company-scrollbar-fg           ((t (:background ,outline))))
         `(company-tooltip-annotation     ((t (:foreground ,inlay :slant italic))))
         `(company-tooltip-common         ((t (:foreground ,primary :weight bold))))
         `(company-tooltip-selection      ((t (:background ,pri-cont :foreground ,on-pri-cont))))
         `(company-tooltip-search         ((t (:inherit match))))
         `(company-preview-common         ((t (:inherit secondary-selection))))

         ;; ── Corfu ────────────────────────────────────────────────────────
         `(corfu-default   ((t (:foreground ,fg0 :background ,bg2))))
         `(corfu-current   ((t (:foreground ,on-pri-cont :background ,pri-cont :weight bold))))
         `(corfu-bar       ((t (:background ,outline))))
         `(corfu-border    ((t (:background ,outline-var))))
         `(corfu-annotations ((t (:foreground ,inlay :slant italic))))

         ;; ── Ivy / Vertico ─────────────────────────────────────────────────
         `(ivy-current-match            ((t (:foreground ,on-pri-cont :background ,pri-cont))))
         `(ivy-minibuffer-match-face-1  ((t (:foreground ,keywords :weight bold))))
         `(ivy-minibuffer-match-face-2  ((t (:foreground ,functions))))
         `(ivy-minibuffer-match-face-3  ((t (:foreground ,types))))
         `(ivy-minibuffer-match-face-4  ((t (:foreground ,constants))))
         `(ivy-confirm-face             ((t (:foreground ,green))))
         `(ivy-match-required-face      ((t (:foreground ,red))))
         `(ivy-virtual                  ((t (:foreground ,fg1))))
         `(ivy-action                   ((t (:foreground ,functions))))

         ;; ── Org mode ─────────────────────────────────────────────────────
         `(org-agenda-structure    ((t (:foreground ,primary :weight bold))))
         `(org-agenda-date         ((t (:foreground ,secondary))))
         `(org-agenda-done         ((t (:foreground ,green))))
         `(org-agenda-dimmed-todo-face ((t (:foreground ,outline))))
         `(org-block               ((t (:foreground ,fg0 :background ,bg1))))
         `(org-block-begin-line    ((t (:foreground ,outline :background ,bg1))))
         `(org-code                ((t (:foreground ,strings))))
         `(org-column              ((t (:background ,bg2))))
         `(org-date                ((t (:foreground ,types :underline t))))
         `(org-document-info       ((t (:foreground ,modules))))
         `(org-document-info-keyword ((t (:foreground ,keywords))))
         `(org-document-title      ((t (:foreground ,primary :weight bold :height 1.44))))
         `(org-done                ((t (:foreground ,green :background ,bg1))))
         `(org-ellipsis            ((t (:foreground ,outline))))
         `(org-footnote            ((t (:foreground ,labels))))
         `(org-formula             ((t (:foreground ,red))))
         `(org-hide                ((t (:foreground ,bg0))))
         `(org-link                ((t (:foreground ,secondary :underline t))))
         `(org-scheduled           ((t (:foreground ,primary))))
         `(org-scheduled-previously ((t (:foreground ,orange))))
         `(org-scheduled-today     ((t (:foreground ,primary :weight bold))))
         `(org-special-keyword     ((t (:foreground ,specials))))
         `(org-table               ((t (:foreground ,on-sec-cont))))
         `(org-todo                ((t (:foreground ,red :background ,bg1))))
         `(org-upcoming-deadline   ((t (:foreground ,orange))))
         `(org-verbatim            ((t (:foreground ,constants))))
         `(org-warning             ((t (:foreground ,red :weight bold))))

         ;; ── Markdown ─────────────────────────────────────────────────────
         `(markdown-url-face   ((t (:inherit link))))
         `(markdown-link-face  ((t (:foreground ,secondary :underline t))))
         `(markdown-code-face  ((t (:foreground ,strings :background ,bg1))))

         ;; ── Rainbow delimiters ────────────────────────────────────────────
         `(rainbow-delimiters-depth-1-face ((t (:foreground ,keywords))))
         `(rainbow-delimiters-depth-2-face ((t (:foreground ,functions))))
         `(rainbow-delimiters-depth-3-face ((t (:foreground ,types))))
         `(rainbow-delimiters-depth-4-face ((t (:foreground ,constants))))
         `(rainbow-delimiters-depth-5-face ((t (:foreground ,macros))))
         `(rainbow-delimiters-depth-6-face ((t (:foreground ,specials))))
         `(rainbow-delimiters-depth-7-face ((t (:foreground ,tags))))
         `(rainbow-delimiters-depth-8-face ((t (:foreground ,modules))))
         `(rainbow-delimiters-depth-9-face ((t (:foreground ,fg0))))

         ;; ── Show paren ────────────────────────────────────────────────────
         `(show-paren-match    ((t (:foreground ,on-primary :background ,primary :weight bold))))
         `(show-paren-mismatch ((t (:foreground ,bg0 :background ,red))))

         ;; ── Avy ──────────────────────────────────────────────────────────
         `(avy-lead-face   ((t (:foreground ,on-primary :background ,primary))))
         `(avy-lead-face-0 ((t (:foreground ,on-secondary :background ,secondary))))
         `(avy-lead-face-1 ((t (:foreground ,on-pri-cont :background ,pri-cont))))
         `(avy-lead-face-2 ((t (:foreground ,on-ter-cont :background ,ter-cont))))
         `(avy-background-face ((t (:foreground ,outline))))
         `(avy-goto-char-timer-face ((t (:inherit highlight))))

         ;; ── Meow ─────────────────────────────────────────────────────────
         `(meow-search-highlight ((t (:foreground ,search-fg :background ,search-bg))))

         ;; ── Whitespace ───────────────────────────────────────────────────
         `(whitespace-empty          ((t (:foreground ,red :background ,bg1))))
         `(whitespace-hspace         ((t (:foreground ,outline :background ,outline))))
         `(whitespace-indentation    ((t (:foreground ,outline :background ,bg1))))
         `(whitespace-line           ((t (:foreground ,orange :background ,bg1))))
         `(whitespace-newline        ((t (:foreground ,outline-var))))
         `(whitespace-space          ((t (:foreground ,outline-var :background ,bg1))))
         `(whitespace-space-after-tab ((t (:foreground ,red :background ,bg1))))
         `(whitespace-space-before-tab ((t (:foreground ,red :background ,bg1))))
         `(whitespace-tab            ((t (:foreground ,outline-var :background ,bg1))))
         `(whitespace-trailing       ((t (:foreground ,red :background ,bg1))))

         ;; ── Term / ANSI ───────────────────────────────────────────────────
         `(term                   ((t (:foreground ,fg0 :background ,bg0))))
         `(term-color-black       ((t (:foreground ,bg0 :background ,bg0))))
         `(term-color-white       ((t (:foreground ,fg0 :background ,fg0))))
         `(term-color-red         ((t (:foreground ,red :background ,red))))
         `(term-color-yellow      ((t (:foreground ,orange :background ,orange))))
         `(term-color-green       ((t (:foreground ,green :background ,green))))
         `(term-color-cyan        ((t (:foreground ,cyan :background ,cyan))))
         `(term-color-blue        ((t (:foreground ,blue :background ,blue))))
         `(term-color-magenta     ((t (:foreground ,specials :background ,specials))))
         `(ansi-color-black       ((t (:foreground ,bg0 :background ,bg0))))
         `(ansi-color-white       ((t (:foreground ,fg0 :background ,fg0))))
         `(ansi-color-red         ((t (:foreground ,red :background ,red))))
         `(ansi-color-yellow      ((t (:foreground ,orange :background ,orange))))
         `(ansi-color-green       ((t (:foreground ,green :background ,green))))
         `(ansi-color-cyan        ((t (:foreground ,cyan :background ,cyan))))
         `(ansi-color-blue        ((t (:foreground ,blue :background ,blue))))
         `(ansi-color-magenta     ((t (:foreground ,specials :background ,specials))))

         ;; ── Custom / Eldoc ────────────────────────────────────────────────
         `(custom-variable-tag    ((t (:foreground ,functions))))
         `(custom-group-tag       ((t (:foreground ,primary :weight bold))))
         `(custom-state           ((t (:foreground ,green))))
         `(eldoc-highlight-function-argument ((t (:foreground ,keywords :weight bold))))

         ;; ── Tooltip ──────────────────────────────────────────────────────
         `(tooltip ((t (:foreground ,fg0 :background ,bg2))))

         ;; ── Ediff ────────────────────────────────────────────────────────
         `(ediff-even-diff-A ((t (:background ,bg1))))
         `(ediff-even-diff-B ((t (:background ,bg1))))
         `(ediff-even-diff-C ((t (:background ,bg1))))
         `(ediff-odd-diff-A  ((t (:foreground ,fg1 :background ,bg2))))
         `(ediff-odd-diff-B  ((t (:foreground ,fg1 :background ,bg2))))
         `(ediff-odd-diff-C  ((t (:foreground ,fg1 :background ,bg2)))))

        (custom-theme-set-variables
         'custom-${name}
         `(ansi-color-names-vector [,bg0 ,red ,green ,orange ,blue ,specials ,cyan ,fg0])))

      ;;;###autoload
      (when load-file-name
        (add-to-list 'custom-theme-load-path
                     (file-name-as-directory (file-name-directory load-file-name))))

      (provide-theme 'custom-${name})
      (provide 'custom-${name}-theme)

      ;;; custom-${name}-theme.el ends here
    '';
}
