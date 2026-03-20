{ paletteSet, ... }:
let
  p = paletteSet.palette;
in
{
  "obsidian/theme.css" =
    #css
    ''
      .theme-dark {
        --background-primary: ${p.bg} !important;
        --background-primary-alt: ${p.surface_container_low} !important;
        --background-secondary: ${p.surface_container_low} !important;
        --background-secondary-alt: ${p.bg} !important;
        --background-modifier-border: ${p.surface_container_high} !important;
        
        --text-normal: ${p.fg} !important;
        --text-muted: ${p.on_surface_variant} !important;
        --text-faint: ${p.outline} !important;
        --text-accent: ${p.secondary} !important;
        --text-on-accent: ${p.bg} !important;
        
        --interactive-accent: ${p.primary} !important;
        --interactive-accent-hover: ${p.primary_fixed_dim} !important;
        --background-modifier-hover: ${p.surface_container} !important;
        --background-modifier-active-hover: ${p.primary_container} !important;
        
        --code-background: ${p.surface_container_low} !important;
        --code-normal: ${p.fg} !important;
        --code-comment: ${p.outline} !important;
        --code-function: ${p.blue} !important;
        --code-keyword: ${p.magenta} !important;
        --code-string: ${p.green} !important;
        --code-value: ${p.orange} !important;
        --code-important: ${p.red} !important;

        --nav-item-color: ${p.fg} !important;
        --nav-item-color-active: ${p.primary} !important;
        --nav-item-background-active: ${p.primary_container} !important;
        
        --tab-background-active: ${p.bg} !important;
        --tab-text-active: ${p.primary} !important;
        --tab-outline-color: ${p.primary} !important;

        --scrollbar-bg: transparent !important;
        --scrollbar-thumb-bg: ${p.surface_container_high} !important;
      }

      /* CM6 */
      .cm-s-obsidian .cm-line { color: ${p.fg}; }

      .cm-s-obsidian .cm-keyword { color: ${p.keywords} !important; }
      .cm-s-obsidian .cm-atom { color: ${p.constants} !important; }
      .cm-s-obsidian .cm-number { color: ${p.numeric} !important; }
      .cm-s-obsidian .cm-def { color: ${p.functions} !important; }
      .cm-s-obsidian .cm-variable { color: ${p.on_background} !important; }
      .cm-s-obsidian .cm-property { color: ${p.functions} !important; }
      .cm-s-obsidian .cm-string { color: ${p.strings} !important; }
      .cm-s-obsidian .cm-string-2 { color: ${p.specials} !important; }
      .cm-s-obsidian .cm-comment { color: ${p.comments} !important; }
      .cm-s-obsidian .cm-variable-2 { color: ${p.types} !important; }
      .cm-s-obsidian .cm-tag { color: ${p.tags} !important; }
      .cm-s-obsidian .cm-meta { color: ${p.modules} !important; }

      /* Excalidraw */
      .excalidraw.theme--dark {
        --color-primary: ${p.primary} !important;
        --color-primary-darker: ${p.primary_container} !important;
        --bg-color: ${p.bg} !important;
        --popup-secondary-bg-color: ${p.surface_container} !important;
        --icon-fill-color: ${p.fg} !important;
        --text-color-primary: ${p.fg} !important;
        --sidebar-border-color: ${p.outline_variant} !important;
      }

      .excalidraw-code-block {
          background-color: ${p.surface_container_low} !important;
          border: 1px solid ${p.outline_variant} !important;
      }

      /* Shiki */
      .markdown-rendered code,
      .markdown-source-view.mod-cm6 .cm-content,
      .shiki {
        --shiki-color-text: ${p.fg} !important;
        --shiki-color-background: ${p.surface_container_low} !important;
      }

      .token.keyword, .cm-keyword { color: ${p.keywords} !important; }
      .token.string, .cm-string { color: ${p.strings} !important; }
      .token.function, .cm-function { color: ${p.functions} !important; }
      .token.number, .cm-number { color: ${p.numeric} !important; }
      .token.comment, .cm-comment { color: ${p.comments} !important; }
      .token.operator, .cm-operator { color: ${p.punctuation} !important; }
      .token.class-name, .cm-type { color: ${p.types} !important; }
      .token.builtin, .cm-builtin { color: ${p.builtins} !important; }
      .token.regex { color: ${p.numeric} !important; }

      .theme-dark .is-active {
          --text-normal: ${p.bg};
      }
    '';
}
