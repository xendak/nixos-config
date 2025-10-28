{ paletteSet, ... }:
let
  p = paletteSet.palette;
  name = "nix";
in
{
  # TODO: maybe we need to start getting surface/selection/etc, like Material Theme ideas
  "emacs/themes/base16-${name}-theme.el" =
    # elisp
    ''
      ;;; base16-${name}-theme.el
      ;;; Standalone base16 theme
      (deftheme base16-${name}
        "A custom base16 theme with extended color support.")

      (defvar base16-theme-colors
        '(:base00       "${p.base00}"
          :base01       "${p.base01}"
          :base02       "${p.base02}"
          :base03       "${p.base03}"
          :base04       "${p.base04}"
          :base05       "${p.base05}"
          :base06       "${p.base06}"
          :base07       "${p.base07}"
          :base08       "${p.base08}"
          :base09       "${p.base09}"
          :base0A       "${p.base0A}"
          :base0B       "${p.base0B}"
          :base0C       "${p.base0C}"
          :base0D       "${p.base0D}"
          :base0E       "${p.base0E}"
          :base0F       "${p.base0F}"
          :region-bg    "${p.accent}"
          :region-fg    "${p.cursor_fg}"
          :cursor-bg    "${p.cursor_bg}"
          :cursor-fg    "${p.cursor_fg}"
          :search-bg    "${p.bg}"
          :search-fg    "${p.fg}"
          :beacon-bg    "${p.base02}"
          :beacon-fg    "${p.base05}")
        "Colors for base16-${name} theme.")

      (defvar base16-default-theme-colors base16-theme-colors
        "Current theme colors.")

      ;; Extract colors for use in theme definition
      (let ((base00 (plist-get base16-theme-colors :base00))
            (base01 (plist-get base16-theme-colors :base01))
            (base02 (plist-get base16-theme-colors :base02))
            (base03 (plist-get base16-theme-colors :base03))
            (base04 (plist-get base16-theme-colors :base04))
            (base05 (plist-get base16-theme-colors :base05))
            (base06 (plist-get base16-theme-colors :base06))
            (base07 (plist-get base16-theme-colors :base07))
            (base08 (plist-get base16-theme-colors :base08))
            (base09 (plist-get base16-theme-colors :base09))
            (base0A (plist-get base16-theme-colors :base0A))
            (base0B (plist-get base16-theme-colors :base0B))
            (base0C (plist-get base16-theme-colors :base0C))
            (base0D (plist-get base16-theme-colors :base0D))
            (base0E (plist-get base16-theme-colors :base0E))
            (base0F (plist-get base16-theme-colors :base0F))
            ;; Custom colors
            (cursor-bg (plist-get base16-theme-colors :cursor-bg))
            (cursor-fg (plist-get base16-theme-colors :cursor-fg))
            (region-bg (plist-get base16-theme-colors :region-bg))
            (region-fg (plist-get base16-theme-colors :region-fg))
            (search-bg (plist-get base16-theme-colors :search-bg))
            (search-fg (plist-get base16-theme-colors :search-fg)))

        (custom-theme-set-faces
         'base16-${name}

         ;;; Built-in
         `(border ((t (:background ,base03))))
         `(cursor ((t (:background ,cursor-bg :foreground ,cursor-fg))))
         `(default ((t (:foreground ,base05 :background ,base00))))
         `(fringe ((t (:background ,base01))))
         `(gui-element ((t (:background ,base01))))
         `(header-line ((t (:foreground ,base0E :background unspecified :inherit mode-line))))
         `(highlight ((t (:background ,base01))))
         `(link ((t (:foreground ,base0D :underline t))))
         `(link-visited ((t (:foreground ,base0E :underline t))))
         `(minibuffer-prompt ((t (:foreground ,base0D))))
         `(region ((t (:background ,region-bg :distant-foreground ,region-fg))))
         `(secondary-selection ((t (:background ,base02 :distant-foreground ,base05))))
         `(trailing-whitespace ((t (:foreground ,base0A :background ,base0C))))
         `(vertical-border ((t (:foreground ,base02))))
         `(widget-button ((t (:underline t))))
         `(widget-field ((t (:background ,base03 :box (:line-width 1 :color ,base06)))))
         `(completions-common-part ((t (:foreground ,base0C))))

         `(error ((t (:foreground ,base08 :weight bold))))
         `(warning ((t (:foreground ,base09 :weight bold))))
         `(success ((t (:foreground ,base0B :weight bold))))
         `(shadow ((t (:foreground ,base03))))

         ;;;; compilation
         `(compilation-column-number ((t (:foreground ,base0A))))
         `(compilation-line-number ((t (:foreground ,base0A))))
         `(compilation-message-face ((t (:foreground ,base0D))))
         `(compilation-mode-line-exit ((t (:foreground ,base0B))))
         `(compilation-mode-line-fail ((t (:foreground ,base08))))
         `(compilation-mode-line-run ((t (:foreground ,base0D))))

         ;;;; custom
         `(custom-variable-tag ((t (:foreground ,base0D))))
         `(custom-group-tag ((t (:foreground ,base0D))))
         `(custom-state ((t (:foreground ,base0B))))

         ;;;; font-lock
         `(font-lock-builtin-face ((t (:foreground ,base0C))))
         `(font-lock-comment-delimiter-face ((t (:foreground ,base03))))
         `(font-lock-comment-face ((t (:foreground ,base03))))
         `(font-lock-constant-face ((t (:foreground ,base09))))
         `(font-lock-doc-face ((t (:foreground ,base04))))
         `(font-lock-doc-string-face ((t (:foreground ,base03))))
         `(font-lock-function-name-face ((t (:foreground ,base0D))))
         `(font-lock-keyword-face ((t (:foreground ,base0E))))
         `(font-lock-negation-char-face ((t (:foreground ,base0B))))
         `(font-lock-preprocessor-face ((t (:foreground ,base0D))))
         `(font-lock-regexp-grouping-backslash ((t (:foreground ,base0A))))
         `(font-lock-regexp-grouping-construct ((t (:foreground ,base0E))))
         `(font-lock-string-face ((t (:foreground ,base0B))))
         `(font-lock-type-face ((t (:foreground ,base0A))))
         `(font-lock-variable-name-face ((t (:foreground ,base08))))
         `(font-lock-warning-face ((t (:foreground ,base08))))

         ;;;; help buffers
         `(help-key-binding ((t (:foreground ,base05 :background ,base01 :box (:line-width 1 :color ,base05) :inherit fixed-pitch))))

         ;;;; isearch
         `(match ((t (:foreground ,base0D :background ,base01 :inverse-video t))))
         `(isearch ((t (:foreground ,search-fg :background ,search-bg :inverse-video t))))
         `(lazy-highlight ((t (:foreground ,base0C :background ,base01 :inverse-video t))))
         `(isearch-lazy-highlight-face ((t (:inherit lazy-highlight))))
         `(isearch-fail ((t (:background ,base01 :inverse-video t :inherit font-lock-warning-face))))

         ;;;; line-numbers
         `(line-number ((t (:foreground ,base03 :background ,base01))))
         `(line-number-current-line ((t (:inherit fringe))))

         ;;;; mode-line
         `(mode-line ((t (:foreground ,base04 :background ,base02))))
         `(mode-line-buffer-id ((t (:foreground ,base0B :background unspecified))))
         `(mode-line-emphasis ((t (:foreground ,base06 :slant italic))))
         `(mode-line-highlight ((t (:foreground ,base0E :box nil :weight bold))))
         `(mode-line-inactive ((t (:foreground ,base03 :background ,base01 :box nil))))

         ;;;; tab-bar
         `(tab-bar ((t (:background ,base01))))
         `(tab-bar-tab ((t (:foreground ,base09 :background ,base01))))
         `(tab-bar-tab-inactive ((t (:foreground ,base06 :background ,base01))))
         `(tab-bar-tab-group-current ((t (:foreground ,base05 :background ,base00))))
         `(tab-bar-tab-group-inactive ((t (:background ,base01))))

         ;;;; tab-line
         `(tab-line ((t (:background ,base01))))
         `(tab-line-tab ((t (:background ,base01))))
         `(tab-line-tab-inactive ((t (:background ,base01))))
         `(tab-line-tab-current ((t (:foreground ,base05 :background ,base00))))
         `(tab-line-highlight ((t (:distant-foreground ,base05 :background ,base02))))

         ;;; Third-party

         ;;;; anzu-mode
         `(anzu-mode-line ((t (:foreground ,base0E))))

         ;;;; avy
         `(avy-lead-face-0 ((t (:foreground ,base00 :background ,base0C))))
         `(avy-lead-face-1 ((t (:foreground ,base00 :background ,base05))))
         `(avy-lead-face-2 ((t (:foreground ,base00 :background ,base0E))))
         `(avy-lead-face ((t (:foreground ,base00 :background ,base09))))
         `(avy-background-face ((t (:foreground ,base03))))
         `(avy-goto-char-timer-face ((t (:inherit highlight))))

         ;;;; company-mode
         `(company-tooltip ((t (:inherit tooltip))))
         `(company-scrollbar-bg ((t (:background ,base07))))
         `(company-scrollbar-fg ((t (:background ,base04))))
         `(company-tooltip-annotation ((t (:foreground ,base08))))
         `(company-tooltip-common ((t (:inherit font-lock-constant-face))))
         `(company-tooltip-selection ((t (:background ,base02 :inherit font-lock-function-name-face))))
         `(company-tooltip-search ((t (:inherit match))))
         `(company-tooltip-search-selection ((t (:inherit match))))
         `(company-preview-common ((t (:inherit secondary-selection))))
         `(company-preview ((t (:foreground ,base04))))
         `(company-preview-search ((t (:inherit match))))
         `(company-echo-common ((t (:inherit secondary-selection))))

         ;;;; corfu
         `(corfu-default ((t (:inherit tooltip))))
         `(corfu-current ((t (:background ,base02 :inherit font-lock-function-name-face))))
         `(corfu-bar ((t (:background ,base05))))
         `(corfu-border ((t (:background ,base04))))

         ;;;; diff-hl-mode
         `(diff-hl-change ((t (:foreground ,base0E))))
         `(diff-hl-delete ((t (:foreground ,base08))))
         `(diff-hl-insert ((t (:foreground ,base0B))))

         ;;;; diff-mode
         `(diff-added ((t (:foreground ,base0B))))
         `(diff-changed ((t (:foreground ,base0E))))
         `(diff-removed ((t (:foreground ,base08))))
         `(diff-header ((t (:background ,base01))))
         `(diff-file-header ((t (:background ,base02))))
         `(diff-hunk-header ((t (:foreground ,base0E :background ,base01))))

         ;;;; dired
         `(dired-directory ((t (:foreground ,base0D :weight bold))))
         `(dired-symlink ((t (:foreground ,base0E))))

         ;;;; ediff-mode
         `(ediff-even-diff-A ((t (:inverse-video t))))
         `(ediff-even-diff-B ((t (:inverse-video t))))
         `(ediff-even-diff-C ((t (:inverse-video t))))
         `(ediff-odd-diff-A ((t (:foreground ,base04 :inverse-video t))))
         `(ediff-odd-diff-B ((t (:foreground ,base04 :inverse-video t))))
         `(ediff-odd-diff-C ((t (:foreground ,base04 :inverse-video t))))

         ;;;; eldoc-mode
         `(eldoc-highlight-function-argument ((t (:foreground ,base0B :weight bold))))

         ;;;; flycheck-mode
         `(flycheck-error ((t (:underline (:style wave :color ,base08)))))
         `(flycheck-info ((t (:underline (:style wave :color ,base0B)))))
         `(flycheck-warning ((t (:underline (:style wave :color ,base09)))))

         ;;;; git-gutter-mode
         `(git-gutter:added ((t (:foreground ,base0B))))
         `(git-gutter:deleted ((t (:foreground ,base08))))
         `(git-gutter:modified ((t (:foreground ,base0E))))

         ;;;; hl-line-mode
         `(hl-line ((t (:background ,base01))))
         `(col-highlight ((t (:background ,base01))))

         ;;;; ido-mode
         `(ido-subdir ((t (:foreground ,base04))))
         `(ido-first-match ((t (:foreground ,base09 :weight bold))))
         `(ido-only-match ((t (:foreground ,base08 :weight bold))))
         `(ido-indicator ((t (:foreground ,base08 :background ,base01))))
         `(ido-virtual ((t (:foreground ,base04))))

         ;;;; ivy-mode
         `(ivy-current-match ((t (:foreground ,base09 :background ,base01))))
         `(ivy-minibuffer-match-face-1 ((t (:foreground ,base0E))))
         `(ivy-minibuffer-match-face-2 ((t (:foreground ,base0D))))
         `(ivy-minibuffer-match-face-3 ((t (:foreground ,base0C))))
         `(ivy-minibuffer-match-face-4 ((t (:foreground ,base0B))))
         `(ivy-confirm-face ((t (:foreground ,base0B))))
         `(ivy-match-required-face ((t (:foreground ,base08))))
         `(ivy-virtual ((t (:foreground ,base04))))
         `(ivy-action ((t (:foreground ,base0D))))

         ;;;; magit
         `(magit-branch ((t (:foreground ,base04 :weight bold))))
         `(magit-branch-current ((t (:foreground ,base0C :weight bold :box t))))
         `(magit-branch-local ((t (:foreground ,base0C :weight bold))))
         `(magit-branch-remote ((t (:foreground ,base0B :weight bold))))
         `(magit-diff-context-highlight ((t (:background ,base01 :foreground ,base05))))
         `(magit-diff-file-header ((t (:background ,base01 :foreground ,base05))))
         `(magit-hash ((t (:foreground ,base0D))))
         `(magit-section-highlight ((t (:background ,base01))))

         ;;;; markdown-mode
         `(markdown-url-face ((t (:inherit link))))
         `(markdown-link-face ((t (:foreground ,base0D :underline t))))

         ;;;; meow (custom addition)
         `(meow-search-highlight ((t (:foreground ,search-fg :background ,search-bg))))

         ;;;; org-mode
         `(org-agenda-structure ((t (:foreground ,base0E))))
         `(org-agenda-date ((t (:foreground ,base0D :underline nil))))
         `(org-agenda-done ((t (:foreground ,base0B))))
         `(org-agenda-dimmed-todo-face ((t (:foreground ,base04))))
         `(org-block ((t (:foreground ,base05 :background ,base01))))
         `(org-block-begin-line ((t (:foreground ,base03 :background ,base01))))
         `(org-code ((t (:foreground ,base0A))))
         `(org-column ((t (:background ,base01))))
         `(org-column-title ((t (:weight bold :underline t :inherit org-column))))
         `(org-date ((t (:foreground ,base0E :underline t))))
         `(org-document-info ((t (:foreground ,base0C))))
         `(org-document-info-keyword ((t (:foreground ,base0B))))
         `(org-document-title ((t (:foreground ,base09 :weight bold :height 1.44))))
         `(org-done ((t (:foreground ,base0B :background ,base01))))
         `(org-ellipsis ((t (:foreground ,base04))))
         `(org-footnote ((t (:foreground ,base0C))))
         `(org-formula ((t (:foreground ,base08))))
         `(org-hide ((t (:foreground ,base03))))
         `(org-link ((t (:foreground ,base0D))))
         `(org-scheduled ((t (:foreground ,base0B))))
         `(org-scheduled-previously ((t (:foreground ,base09))))
         `(org-scheduled-today ((t (:foreground ,base0B))))
         `(org-special-keyword ((t (:foreground ,base09))))
         `(org-table ((t (:foreground ,base0E))))
         `(org-todo ((t (:foreground ,base08 :background ,base01))))
         `(org-upcoming-deadline ((t (:foreground ,base09))))
         `(org-verbatim ((t (:foreground ,base0A))))
         `(org-warning ((t (:foreground ,base08 :weight bold))))

         ;;;; rainbow-delimiters
         `(rainbow-delimiters-depth-1-face ((t (:foreground ,base0E))))
         `(rainbow-delimiters-depth-2-face ((t (:foreground ,base0D))))
         `(rainbow-delimiters-depth-3-face ((t (:foreground ,base0C))))
         `(rainbow-delimiters-depth-4-face ((t (:foreground ,base0B))))
         `(rainbow-delimiters-depth-5-face ((t (:foreground ,base0A))))
         `(rainbow-delimiters-depth-6-face ((t (:foreground ,base09))))
         `(rainbow-delimiters-depth-7-face ((t (:foreground ,base08))))
         `(rainbow-delimiters-depth-8-face ((t (:foreground ,base03))))
         `(rainbow-delimiters-depth-9-face ((t (:foreground ,base05))))

         ;;;; show-paren-mode
         `(show-paren-match ((t (:foreground ,base01 :background ,base0D))))
         `(show-paren-mismatch ((t (:foreground ,base01 :background ,base09))))

         ;;;; term and ansi-term
         `(term ((t (:foreground ,base05 :background ,base00))))
         `(term-color-black ((t (:foreground ,base02 :background ,base00))))
         `(term-color-white ((t (:foreground ,base05 :background ,base07))))
         `(term-color-red ((t (:foreground ,base08 :background ,base08))))
         `(term-color-yellow ((t (:foreground ,base0A :background ,base0A))))
         `(term-color-green ((t (:foreground ,base0B :background ,base0B))))
         `(term-color-cyan ((t (:foreground ,base0C :background ,base0C))))
         `(term-color-blue ((t (:foreground ,base0D :background ,base0D))))
         `(term-color-magenta ((t (:foreground ,base0E :background ,base0E))))

         ;;;; ansi-colors
         `(ansi-color-black ((t (:foreground ,base02 :background ,base00))))
         `(ansi-color-white ((t (:foreground ,base05 :background ,base07))))
         `(ansi-color-red ((t (:foreground ,base08 :background ,base08))))
         `(ansi-color-yellow ((t (:foreground ,base0A :background ,base0A))))
         `(ansi-color-green ((t (:foreground ,base0B :background ,base0B))))
         `(ansi-color-cyan ((t (:foreground ,base0C :background ,base0C))))
         `(ansi-color-blue ((t (:foreground ,base0D :background ,base0D))))
         `(ansi-color-magenta ((t (:foreground ,base0E :background ,base0E))))

         ;;;; tooltip
         `(tooltip ((t (:background ,base01 :inherit default))))

         ;;;; whitespace-mode
         `(whitespace-empty ((t (:foreground ,base08 :background ,base0A))))
         `(whitespace-hspace ((t (:foreground ,base04 :background ,base04))))
         `(whitespace-indentation ((t (:foreground ,base08 :background ,base0A))))
         `(whitespace-line ((t (:foreground ,base0F :background ,base01))))
         `(whitespace-newline ((t (:foreground ,base04))))
         `(whitespace-space ((t (:foreground ,base03 :background ,base01))))
         `(whitespace-space-after-tab ((t (:foreground ,base08 :background ,base0A))))
         `(whitespace-space-before-tab ((t (:foreground ,base08 :background ,base09))))
         `(whitespace-tab ((t (:foreground ,base03 :background ,base01))))
         `(whitespace-trailing ((t (:foreground ,base0A :background ,base08)))))

        ;; ansi
        (custom-theme-set-variables
         'base16-${name}
         `(ansi-color-names-vector [,base00 ,base08 ,base0B ,base0A ,base0D ,base0E ,base0D ,base05])))

      ;;;###autoload
      (when load-file-name
        (add-to-list 'custom-theme-load-path
                     (file-name-as-directory (file-name-directory load-file-name))))

      (provide-theme 'base16-${name})
      (provide 'base16-${name}-theme)

      ;;; base16-${name}-theme.el ends here
          
    '';
}

# "emacs/themes/base16-${name}-theme.el" =
#   # elisp
#   ''
#     (require 'base16-theme)
#     (defvar base16-${name}-theme-colors
#       '(:base00 "${p.base00}"
#         :base01 "${p.base01}"
#         :base02 "${p.base02}"
#         :base03 "${p.base03}"
#         :base04 "${p.base04}"
#         :base05 "${p.base05}"
#         :base06 "${p.base06}"
#         :base07 "${p.base07}"
#         :base08 "${p.base08}"
#         :base09 "${p.base09}"
#         :base0A "${p.base0A}"
#         :base0B "${p.base0B}"
#         :base0C "${p.base0C}"
#         :base0D "${p.base0D}"
#         :base0E "${p.base0E}"
#         :base0F "${p.base0F}"
#         :region-bg  "${p.accent}"
#         :region-fg  "${p.cursor_fg}"
#         :cursor-bg  "${p.cursor_bg}"
#         :cursor-fg  "${p.cursor_fg}"
#         :meow-bg    "${p.bg}"
#         :meow-fg    "${p.fg}")
#       "Colors for base16-${name} theme.")

#     (deftheme base16-${name})
#     (base16-theme-define 'base16-${name} base16-${name}-theme-colors)

#     (provide-theme 'base16-${name})
#     (provide 'base16-${name}-theme)
#   '';
