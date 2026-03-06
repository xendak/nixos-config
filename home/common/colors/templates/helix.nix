{ paletteSet, lib, ... }:
let
  p = paletteSet.palette;

  formatPalette =
    palette:
    lib.concatStringsSep "\n" (lib.mapAttrsToList (name: value: ''${name} = "${value}"'') palette);
in
{
  "helix/themes/current.toml" = ''
    "attributes" = "${p.orange}"
    "constant" = "${p.orange}"
    "constant.character.escape" = "${p.cyan}"
    "constant.numeric" = "${p.orange}"
    "constructor" = "${p.blue}"
    "debug" = "${p.on_surface_variant}"

    "diff.delta" = "${p.orange}"
    "diff.minus" = "${p.red}"
    "diff.plus" = "${p.green}"
    "error" = "${p.red}"
    "function" = "${p.blue}"
    "hint" = "${p.on_surface_variant}"
    "info" = "${p.blue}"
    "keyword" = "${p.magenta}"
    "label" = "${p.magenta}"
    "namespace" = "${p.magenta}"
    "operator" = "${p.on_surface}"
    "special" = "${p.blue}"
    "string" = "${p.green}"
    "type" = "${p.yellow}"
    "variable" = "${p.red}"
    "variable.other.member" = "${p.red}"
    "warning" = "${p.orange}"

    ["ui.virtual.inlay-hint"]
    fg = "${p.tertiary_container}"

    [comment]
    fg = "${p.on_surface_variant}"
    modifiers = ["italic"]

    [diagnostic]
    modifiers = ["underlined"]

    ["diagnostic.error".underline]
    style = "curl"
    color = "${p.red}"

    ["diagnostic.hint".underline]
    style = "curl"
    color = "${p.cyan}"

    ["diagnostic.info".underline]
    style = "curl"
    color = "${p.blue}"

    ["diagnostic.warning".underline]
    style = "curl"
    color = "${p.orange}"

    ["markup.bold"]
    fg = "${p.yellow}"
    modifiers = ["bold"]

    ["markup.italic"]
    fg = "${p.magenta}"
    modifiers = ["italic"]

    ["markup.link.text"]
    fg = "${p.red}"

    ["markup.link.url"]
    fg = "${p.orange}"
    modifiers = ["underlined"]

    ["markup.list"]
    fg = "${p.red}"
    ["markup.quote"]
    fg = "${p.cyan}"
    ["markup.raw"]
    fg = "${p.green}"
    ["markup.strikethrough"]
    modifiers = ["crossed_out"]

    ["ui.background"]
    bg = "${p.surface}"

    ["ui.bufferline"]
    fg = "${p.on_primary_container}"
    bg = "${p.primary_container}"

    ["ui.bufferline.active"]
    fg = "${p.surface}"
    bg = "${p.on_surface_variant}"
    modifiers = ["bold"]

    ["ui.cursor"]
    fg = "${p.cursor_fg}"
    bg = "${p.cursor_bg}"

    ["ui.cursor.insert"]
    fg = "${p.yellow}"
    modifiers = ["underlined"]

    ["ui.cursor.match"]
    fg = "${p.yellow}"
    modifiers = ["underlined"]

    ["ui.cursorline.primary"]
    fg = "${p.on_secondary_container}"
    bg = "${p.secondary_container}"

    ["ui.gutter"]
    bg = "${p.surface}"

    ["ui.help"]
    fg = "${p.on_secondary_container}"
    bg = "${p.secondary_container}"

    ["ui.linenr"]
    fg = "${p.on_surface_variant}"
    bg = "${p.surface}"

    ["ui.linenr.selected"]
    fg = "${p.on_primary_container}"
    bg = "${p.secondary_container}"
    modifiers = ["bold"]

    ["ui.menu"]
    fg = "${p.on_surface}"
    bg = "${p.secondary_container}"

    ["ui.menu.selected"]
    fg = "${p.secondary_container}"
    bg = "${p.on_primary_container}"

    ["ui.popup"]
    bg = "${p.surface_dim}"
    fg = "${p.on_surface}"

    ["ui.selection"]
    bg = "${p.selection_bg}"
    fg = "${p.selection_fg}"

    ["ui.statusline"]
    fg = "${p.on_surface}"
    bg = "${p.surface_container}"

    ["ui.statusline.inactive"]
    bg = "${p.surface_variant}"
    fg = "${p.on_surface_variant}"

    ["ui.statusline.insert"]
    fg = "${p.on_primary}"
    bg = "${p.primary}"

    ["ui.statusline.normal"]
    fg = "${p.on_primary}"
    bg = "${p.magenta}"

    ["ui.statusline.select"]
    fg = "${p.on_primary}"
    bg = "${p.magenta}"

    ["ui.text"]
    fg = "${p.on_surface}"

    ["ui.virtual.indent-guide"]
    fg = "${p.outline_variant}"

    ["ui.virtual.ruler"]
    bg = "${p.secondary_container}"

    ["ui.window"]
    bg = "${p.secondary_container}"

    [palette]
    ${formatPalette p}
  '';
}
