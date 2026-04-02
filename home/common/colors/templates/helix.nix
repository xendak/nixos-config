{ paletteSet, lib, ... }:
let
  p = paletteSet.palette;

  formatPalette =
    palette:
    lib.concatStringsSep "\n" (lib.mapAttrsToList (name: value: ''${name} = "${value}"'') palette);
in
{
  "helix/themes/current.toml" =
    #toml
    ''
      # :Syntax
      "annotation" = "modules"
      "attribute" = "modules"

      "comment" = { fg = "comments", modifiers = ["italic"] }

      "constant" = "constants"
      "constant.character" = "tags"
      "constant.character.escape" = "punctuation"
      "constant.numeric" = "numeric"
      "constant.macro" = "macros"
      "constructor" = "constants"

      "definition" = { underline = { color = "modules", style = "line" } }

      # :System / :Diagnostic
      "error" = "red"
      "warning" = "orange"
      "info" = "blue"
      "hint" = "cyan"

      "diagnostic" = { underline = { color = "punctuation", style = "curl" } }
      "diagnostic.error" = { underline = { color = "red", style = "curl" } }
      "diagnostic.warning" = { underline = { color = "orange", style = "curl" } }
      "diagnostic.info" = { underline = { color = "blue", style = "curl" } }
      "diagnostic.hint" = { underline = { color = "cyan", style = "curl" } }

      "diff.delta" = "yellow"
      "diff.minus" = "red"
      "diff.plus" = "green"

      # :Programming
      "function" = "functions"
      "function.builtin" = "builtins"
      "function.macro" = "macros"

      "keyword" = "keywords"
      "keyword.control.import" = "modules"
      "label" = "labels"

      "module" = "modules"
      "namespace" = "on_background"
      "operator" = "constants"
      "punctuation" = "punctuation"
      "special" = "specials"

      "string" = "strings"
      "string.regexp" = "numeric"
      "string.special" = "specials"
      "string.symbol" = "builtins"

      "tag" = "tags"
      "type" = "types"

      "variable" = "on_background"
      "variable.builtin" = "numeric"
      "variable.other.member" = "on_background"
      "variable.parameter" = "functions"

      # :UI
      "ui.background" = { bg = "background" }
      "ui.text" = { fg = "on_background" }

      "ui.window" = { fg = "outline", bg = "surface_container_low" }

      "ui.linenr" = { fg = "gray", bg = "surface" }
      "ui.linenr.selected" = { fg = "on_primary_container", bg = "secondary_container", modifiers = ["bold"] }

      "ui.gutter" = { bg = "background" }

      # :Picker
      "ui.text.focus" = { bg = "surface_container_high" }
      "ui.picker.header" = { bg = "surface_container_low", fg = "on_surface" }
      "ui.picker.header.column" = "primary"
      # keeping vivid theme similar
      "ui.text.directory" = "blue"

      # :Virtual
      "ui.virtual.indent-guide" = { fg = "surface_container_high" }
      "ui.virtual.inlay-hint" = { fg = "inlay", modifiers = ["italic"] }
      "ui.virtual.jump-label" = { fg = "cursor_fg", bg = "cursor_bg" }

      "ui.menu" = { fg = "on_surface", bg = "surface_container" }
      "ui.menu.selected" = { fg = "on_primary_container", bg = "primary_container", modifiers = ["bold"] }

      "ui.popup" = { bg = "surface_container", fg = "on_surface" }

      "ui.statusline" = { fg = "on_surface", bg = "surface_container_low" }
      "ui.statusline.inactive" = { fg = "outline_variant", bg = "surface_container_lowest" }

      "ui.statusline.normal" = { fg = "on_primary", bg = "primary" }
      "ui.statusline.insert" = { fg = "on_tertiary", bg = "tertiary" }
      "ui.statusline.select" = { fg = "on_secondary", bg = "secondary" }

      "ui.selection" = { bg = "selection_bg", fg = "selection_fg" }
      "ui.highlight" = { bg = "surface_container_low", fg = "selection_fg" }

      "ui.cursorline" = { bg = "surface_container_low" }
      "ui.cursor" = { fg = "cursor_fg", bg = "blue" }
      "ui.cursor.primary" = { fg = "cursor_fg", bg = "cursor_bg" }
      "ui.cursor.match" = { fg = "on_primary", bg = "primary", modifiers = ["underlined"] }

      # :Markup
      "markup.bold" = { fg = "builtins", modifiers = ["bold"] }
      "markup.italic" = { fg = "specials", modifiers = ["italic"] }
      "markup.link.text" = "labels"
      "markup.link.url" = { fg = "strings", modifiers = ["underlined"] }
      "markup.list" = "labels"
      "markup.quote" = "tags"
      "markup.raw" = "keywords"

      [palette]
      ${formatPalette p}
    '';
}
