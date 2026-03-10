{ paletteSet, lib, ... }:
let
  p = paletteSet.palette;

  formatPalette =
    palette:
    lib.concatStringsSep "\n" (lib.mapAttrsToList (name: value: ''${name} = "${value}"'') palette);
in
{
  "helix/themes/current.toml" = ''
    "attributes" = "orange"
    "constant" = "orange"
    "constant.character.escape" = "cyan"
    "constant.numeric" = "orange"
    "constructor" = "blue"
    "debug" = "on_surface_variant"

    "diff.delta" = "orange"
    "diff.minus" = "red"
    "diff.plus" = "green"
    "error" = "red"
    "function" = "blue"
    "hint" = "on_surface_variant"
    "info" = "blue"
    "keyword" = "magenta"
    "label" = "magenta"
    "namespace" = "magenta"
    "operator" = "on_surface"
    "special" = "blue"
    "string" = "green"
    "type" = "yellow"
    "variable" = "red"
    "variable.other.member" = "red"
    "warning" = "orange"

    ["ui.virtual.inlay-hint"]
    fg = "tertiary_container"

    ["ui.virtual.jump-label"]
    fg = "on_primary"
    bg = "primary"

    [comment]
    fg = "on_surface_variant"
    modifiers = ["italic"]

    [diagnostic]
    modifiers = ["underlined"]

    ["diagnostic.error".underline]
    style = "curl"
    color = "red"

    ["diagnostic.hint".underline]
    style = "curl"
    color = "cyan"

    ["diagnostic.info".underline]
    style = "curl"
    color = "blue"

    ["diagnostic.warning".underline]
    style = "curl"
    color = "orange"

    ["markup.bold"]
    fg = "yellow"
    modifiers = ["bold"]

    ["markup.italic"]
    fg = "magenta"
    modifiers = ["italic"]

    ["markup.link.text"]
    fg = "red"

    ["markup.link.url"]
    fg = "orange"
    modifiers = ["underlined"]

    ["markup.list"]
    fg = "red"
    ["markup.quote"]
    fg = "cyan"
    ["markup.raw"]
    fg = "green"
    ["markup.strikethrough"]
    modifiers = ["crossed_out"]

    ["ui.background"]
    bg = "surface"

    ["ui.bufferline"]
    fg = "on_primary_container"
    bg = "primary_container"

    ["ui.bufferline.active"]
    fg = "surface"
    bg = "on_surface_variant"
    modifiers = ["bold"]

    ["ui.cursor"]
    fg = "cursor_fg"
    bg = "cursor_bg"

    ["ui.cursor.insert"]
    fg = "yellow"
    modifiers = ["underlined"]

    ["ui.cursor.match"]
    fg = "yellow"
    modifiers = ["underlined"]

    ["ui.cursorline.primary"]
    fg = "on_secondary_container"
    bg = "secondary_container"

    ["ui.gutter"]
    bg = "surface"

    ["ui.help"]
    fg = "on_secondary_container"
    bg = "secondary_container"

    ["ui.linenr"]
    fg = "on_surface_variant"
    bg = "surface"

    ["ui.linenr.selected"]
    fg = "on_primary_container"
    bg = "secondary_container"
    modifiers = ["bold"]

    ["ui.menu"]
    fg = "on_surface"
    bg = "secondary_container"

    ["ui.menu.selected"]
    fg = "secondary_container"
    bg = "on_primary_container"

    ["ui.popup"]
    bg = "on_surface_low"
    fg = "on_surface"

    ["ui.selection"]
    bg = "selection_bg"
    fg = "selection_fg"

    ["ui.statusline"]
    fg = "on_surface"
    bg = "surface_container"

    ["ui.statusline.inactive"]
    bg = "surface_variant"
    fg = "on_surface_variant"

    ["ui.statusline.insert"]
    fg = "on_primary"
    bg = "primary"

    ["ui.statusline.normal"]
    fg = "on_primary"
    bg = "magenta"

    ["ui.statusline.select"]
    fg = "on_primary"
    bg = "magenta"

    ["ui.text"]
    fg = "on_surface"

    ["ui.virtual.indent-guide"]
    fg = "outline_variant"

    ["ui.virtual.ruler"]
    bg = "secondary_container"

    ["ui.window"]
    bg = "secondary_container"

    [palette]
    ${formatPalette p}
  '';
}
