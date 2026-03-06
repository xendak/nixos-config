{ paletteSet, ... }:
let
  p = paletteSet.palette;
  name = "nix";
in
{
  #   surface roles       → bg / bg-alt / base0-5
  #   on_surface*         → fg / fg-alt
  #   primary             → keywords, accents, links
  #   secondary           → functions, method names
  #   tertiary            → types, labels, markup
  #   on_*_container      → strings (on_primary_container), constants (on_tertiary_container)
  #   error               → errors / warnings
  #   outline*            → comments, borders, subtle chrome
  "emacs/themes/base16-${name}-theme.el" =
    # elisp
    ''
      ;;; base16-${name}-theme.el
      ;;; Material Design 3 theme for Emacs – auto-generated
      (deftheme base16-${name}
        "A Material You themed Emacs theme.")

      (defvar base16-theme-colors
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
          :ter-cont     "${p.tertiary_container}"
          :on-ter-cont  "${p.on_tertiary_container}"
          :error        "${p.error}"
          :on-error     "${p.on_error}"
          :err-cont     "${p.error_container}"
          :on-err-cont  "${p.on_error_container}"
          :outline      "${p.outline}"
          :outline-var  "${p.outline_variant}"
          :inv-surf     "${p.inverse_surface}"
          :inv-on-surf  "${p.inverse_on_surface}"
          :cursor-bg    "${p.palette.cursor_bg or p.primary}"
          :cursor-fg    "${p.palette.cursor_fg or p.on_primary}"
          :region-bg    "${p.primary_container}"
          :region-fg    "${p.on_primary_container}"
          :search-bg    "${p.secondary_container}"
          :search-fg    "${p.on_secondary_container}")
        "Material Design 3 colors for base16-${name} theme.")

      (let ((bg0         (plist-get base16-theme-colors :bg0))
            (bg1         (plist-get base16-theme-colors :bg1))
            (bg2         (plist-get base16-theme-colors :bg2))
            (bg3         (plist-get base16-theme-colors :bg3))
            (bg4         (plist-get base16-theme-colors :bg4))
            (fg0         (plist-get base16-theme-colors :fg0))
            (fg1         (plist-get base16-theme-colors :fg1))
            (fg2         (plist-get base16-theme-colors :fg2))
            (primary     (plist-get base16-theme-colors :primary))
            (on-primary  (plist-get base16-theme-colors :on-primary))
            (pri-cont    (plist-get base16-theme-colors :pri-cont))
            (on-pri-cont (plist-get base16-theme-colors :on-pri-cont))
            (secondary   (plist-get base16-theme-colors :secondary))
            (on-secondary (plist-get base16-theme-colors :on-secondary))
            (sec-cont    (plist-get base16-theme-colors :sec-cont))
            (on-sec-cont (plist-get base16-theme-colors :on-sec-cont))
            (tertiary    (plist-get base16-theme-colors :tertiary))
            (ter-cont    (plist-get base16-theme-colors :ter-cont))
            (on-ter-cont (plist-get base16-theme-colors :on-ter-cont))
            (error       (plist-get base16-theme-colors :error))
            (on-error    (plist-get base16-theme-colors :on-error))
            (err-cont    (plist-get base16-theme-colors :err-cont))
            (on-err-cont (plist-get base16-theme-colors :on-err-cont))
            (outline     (plist-get base16-theme-colors :outline))
            (outline-var (plist-get base16-theme-colors :outline-var))
            (inv-surf    (plist-get base16-theme-colors :inv-surf))
            (inv-on-surf (plist-get base16-theme-colors :inv-on-surf))
            (cursor-bg   (plist-get base16-theme-colors :cursor-bg))
            (cursor-fg   (plist-get base16-theme-colors :cursor-fg))
            (region-bg   (plist-get base16-theme-colors :region-bg))
            (region-fg   (plist-get base16-theme-colors :region-fg))
            (search-bg   (plist-get base16-theme-colors :search-bg))
            (search-fg   (plist-get base16-theme-colors :search-fg)))

        (custom-theme-set-faces
         'base16-${name}

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
         `(trailing-whitespace ((t (:background ,err-cont))))

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

         `(error          ((t (:foreground ,error :weight bold))))
         `(warning        ((t (:foreground ,tertiary :weight bold))))
         `(success        ((t (:foreground ,primary :weight bold))))
         `(shadow         ((t (:foreground ,outline))))

         ;; ── Line numbers ──────────────────────────────────────────────────
         `(line-number              ((t (:foreground ,outline :background ,bg1))))
         `(line-number-current-line ((t (:foreground ,primary :background ,bg2 :weight bold))))

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
         ;; comments → outline (muted)
         `(font-lock-comment-face          ((t (:foreground ,outline :slant italic))))
         `(font-lock-comment-delimiter-face ((t (:foreground ,outline-var :slant italic))))
         `(font-lock-doc-face              ((t (:foreground ,fg2 :slant italic))))
         `(font-lock-doc-string-face       ((t (:foreground ,fg2))))
         ;; strings → on_primary_container
         `(font-lock-string-face           ((t (:foreground ,on-pri-cont))))
         ;; keywords → primary
         `(font-lock-keyword-face          ((t (:foreground ,primary :weight bold))))
         `(font-lock-builtin-face          ((t (:foreground ,primary))))
         `(font-lock-preprocessor-face     ((t (:foreground ,primary))))
         ;; functions → secondary
         `(font-lock-function-name-face    ((t (:foreground ,secondary))))
         ;; variables → on_surface
         `(font-lock-variable-name-face    ((t (:foreground ,fg0))))
         ;; types → tertiary
         `(font-lock-type-face             ((t (:foreground ,tertiary))))
         ;; constants / numbers → on_tertiary_container
         `(font-lock-constant-face         ((t (:foreground ,on-ter-cont))))
         ;; misc
         `(font-lock-regexp-grouping-backslash  ((t (:foreground ,on-sec-cont))))
         `(font-lock-regexp-grouping-construct  ((t (:foreground ,on-sec-cont :weight bold))))
         `(font-lock-negation-char-face         ((t (:foreground ,error))))
         `(font-lock-warning-face               ((t (:foreground ,error :weight bold))))

         ;; ── Search & match ────────────────────────────────────────────────
         `(match           ((t (:foreground ,on-pri-cont :background ,pri-cont))))
         `(isearch         ((t (:foreground ,search-fg :background ,search-bg :inverse-video t))))
         `(lazy-highlight  ((t (:foreground ,on-sec-cont :background ,sec-cont :inverse-video t))))
         `(isearch-lazy-highlight-face ((t (:inherit lazy-highlight))))
         `(isearch-fail    ((t (:background ,err-cont :foreground ,on-err-cont))))
         `(completions-common-part ((t (:foreground ,secondary :weight bold))))

         ;; ── Help ─────────────────────────────────────────────────────────
         `(help-key-binding ((t (:foreground ,fg0 :background ,bg2
                                 :box (:line-width 1 :color ,outline-var)
                                 :inherit fixed-pitch))))

         ;; ── Dired ─────────────────────────────────────────────────────────
         `(dired-directory ((t (:foreground ,secondary :weight bold))))
         `(dired-symlink   ((t (:foreground ,tertiary))))

         ;; ── Compilation ───────────────────────────────────────────────────
         `(compilation-column-number   ((t (:foreground ,on-ter-cont))))
         `(compilation-line-number     ((t (:foreground ,on-ter-cont))))
         `(compilation-message-face    ((t (:foreground ,secondary))))
         `(compilation-mode-line-exit  ((t (:foreground ,primary))))
         `(compilation-mode-line-fail  ((t (:foreground ,error))))
         `(compilation-mode-line-run   ((t (:foreground ,secondary))))

         ;; ── Diff ─────────────────────────────────────────────────────────
         `(diff-added      ((t (:foreground ,on-pri-cont :background ,pri-cont))))
         `(diff-changed    ((t (:foreground ,on-ter-cont :background ,ter-cont))))
         `(diff-removed    ((t (:foreground ,on-err-cont :background ,err-cont))))
         `(diff-header     ((t (:background ,bg2))))
         `(diff-file-header ((t (:background ,bg3 :weight bold))))
         `(diff-hunk-header ((t (:foreground ,tertiary :background ,bg2))))

         ;; ── Diff-hl ───────────────────────────────────────────────────────
         `(diff-hl-change  ((t (:foreground ,tertiary))))
         `(diff-hl-delete  ((t (:foreground ,error))))
         `(diff-hl-insert  ((t (:foreground ,primary))))

         ;; ── Git gutter ────────────────────────────────────────────────────
         `(git-gutter:added    ((t (:foreground ,primary))))
         `(git-gutter:deleted  ((t (:foreground ,error))))
         `(git-gutter:modified ((t (:foreground ,tertiary))))

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
         `(flycheck-error   ((t (:underline (:style wave :color ,error)))))
         `(flycheck-warning ((t (:underline (:style wave :color ,tertiary)))))
         `(flycheck-info    ((t (:underline (:style wave :color ,secondary)))))

         ;; ── Company ──────────────────────────────────────────────────────
         `(company-tooltip                ((t (:foreground ,fg0 :background ,bg2))))
         `(company-scrollbar-bg           ((t (:background ,bg3))))
         `(company-scrollbar-fg           ((t (:background ,outline))))
         `(company-tooltip-annotation     ((t (:foreground ,outline))))
         `(company-tooltip-common         ((t (:foreground ,primary :weight bold))))
         `(company-tooltip-selection      ((t (:background ,pri-cont :foreground ,on-pri-cont))))
         `(company-tooltip-search         ((t (:inherit match))))
         `(company-preview-common         ((t (:inherit secondary-selection))))

         ;; ── Corfu ────────────────────────────────────────────────────────
         `(corfu-default   ((t (:foreground ,fg0 :background ,bg2))))
         `(corfu-current   ((t (:foreground ,on-pri-cont :background ,pri-cont :weight bold))))
         `(corfu-bar       ((t (:background ,outline))))
         `(corfu-border    ((t (:background ,outline-var))))

         ;; ── Ivy / Vertico ─────────────────────────────────────────────────
         `(ivy-current-match            ((t (:foreground ,on-pri-cont :background ,pri-cont))))
         `(ivy-minibuffer-match-face-1  ((t (:foreground ,primary :weight bold))))
         `(ivy-minibuffer-match-face-2  ((t (:foreground ,secondary))))
         `(ivy-minibuffer-match-face-3  ((t (:foreground ,tertiary))))
         `(ivy-minibuffer-match-face-4  ((t (:foreground ,on-ter-cont))))
         `(ivy-confirm-face             ((t (:foreground ,primary))))
         `(ivy-match-required-face      ((t (:foreground ,error))))
         `(ivy-virtual                  ((t (:foreground ,fg1))))
         `(ivy-action                   ((t (:foreground ,secondary))))

         ;; ── Org mode ─────────────────────────────────────────────────────
         `(org-agenda-structure    ((t (:foreground ,primary :weight bold))))
         `(org-agenda-date         ((t (:foreground ,secondary))))
         `(org-agenda-done         ((t (:foreground ,primary))))
         `(org-agenda-dimmed-todo-face ((t (:foreground ,outline))))
         `(org-block               ((t (:foreground ,fg0 :background ,bg1))))
         `(org-block-begin-line    ((t (:foreground ,outline :background ,bg1))))
         `(org-code                ((t (:foreground ,on-ter-cont))))
         `(org-column              ((t (:background ,bg2))))
         `(org-date                ((t (:foreground ,tertiary :underline t))))
         `(org-document-info       ((t (:foreground ,secondary))))
         `(org-document-info-keyword ((t (:foreground ,primary))))
         `(org-document-title      ((t (:foreground ,primary :weight bold :height 1.44))))
         `(org-done                ((t (:foreground ,primary :background ,bg1))))
         `(org-ellipsis            ((t (:foreground ,outline))))
         `(org-footnote            ((t (:foreground ,on-sec-cont))))
         `(org-formula             ((t (:foreground ,error))))
         `(org-hide                ((t (:foreground ,bg0))))
         `(org-link                ((t (:foreground ,secondary :underline t))))
         `(org-scheduled           ((t (:foreground ,primary))))
         `(org-scheduled-previously ((t (:foreground ,tertiary))))
         `(org-scheduled-today     ((t (:foreground ,primary :weight bold))))
         `(org-special-keyword     ((t (:foreground ,on-ter-cont))))
         `(org-table               ((t (:foreground ,on-sec-cont))))
         `(org-todo                ((t (:foreground ,error :background ,bg1))))
         `(org-upcoming-deadline   ((t (:foreground ,tertiary))))
         `(org-verbatim            ((t (:foreground ,on-pri-cont))))
         `(org-warning             ((t (:foreground ,error :weight bold))))

         ;; ── Markdown ─────────────────────────────────────────────────────
         `(markdown-url-face   ((t (:inherit link))))
         `(markdown-link-face  ((t (:foreground ,secondary :underline t))))

         ;; ── Rainbow delimiters ────────────────────────────────────────────
         `(rainbow-delimiters-depth-1-face ((t (:foreground ,primary))))
         `(rainbow-delimiters-depth-2-face ((t (:foreground ,secondary))))
         `(rainbow-delimiters-depth-3-face ((t (:foreground ,tertiary))))
         `(rainbow-delimiters-depth-4-face ((t (:foreground ,on-pri-cont))))
         `(rainbow-delimiters-depth-5-face ((t (:foreground ,on-sec-cont))))
         `(rainbow-delimiters-depth-6-face ((t (:foreground ,on-ter-cont))))
         `(rainbow-delimiters-depth-7-face ((t (:foreground ,error))))
         `(rainbow-delimiters-depth-8-face ((t (:foreground ,outline))))
         `(rainbow-delimiters-depth-9-face ((t (:foreground ,fg0))))

         ;; ── Show paren ────────────────────────────────────────────────────
         `(show-paren-match    ((t (:foreground ,on-primary :background ,primary :weight bold))))
         `(show-paren-mismatch ((t (:foreground ,on-error :background ,error))))

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
         `(whitespace-empty          ((t (:foreground ,error :background ,err-cont))))
         `(whitespace-hspace         ((t (:foreground ,outline :background ,outline))))
         `(whitespace-indentation    ((t (:foreground ,outline :background ,bg1))))
         `(whitespace-line           ((t (:foreground ,on-err-cont :background ,bg1))))
         `(whitespace-newline        ((t (:foreground ,outline-var))))
         `(whitespace-space          ((t (:foreground ,outline-var :background ,bg1))))
         `(whitespace-space-after-tab ((t (:foreground ,error :background ,err-cont))))
         `(whitespace-space-before-tab ((t (:foreground ,error :background ,err-cont))))
         `(whitespace-tab            ((t (:foreground ,outline-var :background ,bg1))))
         `(whitespace-trailing       ((t (:foreground ,on-err-cont :background ,err-cont))))

         ;; ── Term ─────────────────────────────────────────────────────────
         `(term                   ((t (:foreground ,fg0 :background ,bg0))))
         `(term-color-black       ((t (:foreground ,bg0 :background ,bg0))))
         `(term-color-white       ((t (:foreground ,fg0 :background ,fg0))))
         `(term-color-red         ((t (:foreground ,error :background ,error))))
         `(term-color-yellow      ((t (:foreground ,tertiary :background ,tertiary))))
         `(term-color-green       ((t (:foreground ,primary :background ,primary))))
         `(term-color-cyan        ((t (:foreground ,on-pri-cont :background ,on-pri-cont))))
         `(term-color-blue        ((t (:foreground ,secondary :background ,secondary))))
         `(term-color-magenta     ((t (:foreground ,on-sec-cont :background ,on-sec-cont))))
         `(ansi-color-black       ((t (:foreground ,bg0 :background ,bg0))))
         `(ansi-color-white       ((t (:foreground ,fg0 :background ,fg0))))
         `(ansi-color-red         ((t (:foreground ,error :background ,error))))
         `(ansi-color-yellow      ((t (:foreground ,tertiary :background ,tertiary))))
         `(ansi-color-green       ((t (:foreground ,primary :background ,primary))))
         `(ansi-color-cyan        ((t (:foreground ,on-pri-cont :background ,on-pri-cont))))
         `(ansi-color-blue        ((t (:foreground ,secondary :background ,secondary))))
         `(ansi-color-magenta     ((t (:foreground ,on-sec-cont :background ,on-sec-cont))))

         ;; ── Custom / Eldoc ────────────────────────────────────────────────
         `(custom-variable-tag    ((t (:foreground ,secondary))))
         `(custom-group-tag       ((t (:foreground ,primary :weight bold))))
         `(custom-state           ((t (:foreground ,primary))))
         `(eldoc-highlight-function-argument ((t (:foreground ,primary :weight bold))))

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
         'base16-${name}
         `(ansi-color-names-vector [,bg0 ,error ,primary ,tertiary ,secondary ,on-sec-cont ,on-pri-cont ,fg0])))

      ;;;###autoload
      (when load-file-name
        (add-to-list 'custom-theme-load-path
                     (file-name-as-directory (file-name-directory load-file-name))))

      (provide-theme 'base16-${name})
      (provide 'base16-${name}-theme)

      ;;; base16-${name}-theme.el ends here
    '';
}
