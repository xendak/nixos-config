{ paletteSet, lib, ... }:
let
  p = paletteSet.palette;
  formatPalette =
    palette:
    lib.concatStringsSep "\n" (lib.mapAttrsToList (name: value: ''${name} = "${value}"'') palette);
in
{
  "helix/themes/current.toml" = ''
    attributes = "base09"
    constant = "base09"
    "constant.character.escape" = "base08"
    "constant.numeric" = "base09"
    constructor = "base0D"
    debug = "base03"
    "diff.delta" = "base09"
    "diff.minus" = "base08"
    "diff.plus" = "base0B"
    error = "base08"
    function = "base0D"
    hint = "base03"
    info = "base0D"
    keyword = "base0E"
    label = "base0E"
    "markup.heading" = "base0D"
    "markup.link.text" = "base08"
    "markup.list" = "base08"
    "markup.quote" = "base0C"
    "markup.raw" = "base0B"
    namespace = "base0E"
    operator = "base05"
    special = "base0D"
    string = "base0B"
    type = "base0A"
    "ui.cursorline" = "base01"
    "ui.text" = "base05"
    "ui.text.focus" = "base05"
    variable = "base08"
    "variable.other.member" = "base08"
    warning = "base09"

    [comment]
    fg = "base03"
    modifiers = ["bold"]

    [diagnostic]
    modifiers = ["underlined"]

    ["diagnostic.error".underline]
    style = "curl"

    ["diagnostic.hint".underline]
    style = "curl"

    ["diagnostic.info".underline]
    style = "curl"

    ["diagnostic.warning".underline]
    style = "curl"

    ["markup.bold"]
    fg = "base0A"
    modifiers = ["bold"]

    ["markup.italic"]
    fg = "base0E"
    modifiers = ["italic"]

    ["markup.link.url"]
    fg = "base09"
    modifiers = ["underlined"]

    ["markup.strikethrough"]
    modifiers = ["crossed_out"]

    [palette]
    ${formatPalette p}

    ["ui.background"]
    bg = "base00"

    ["ui.bufferline"]
    bg = "base00"
    fg = "base04"

    ["ui.bufferline.active"]
    bg = "base03"
    fg = "base00"
    modifiers = ["bold"]

    ["ui.cursor"]
    bg = "cursor_bg"
    fg = "cursor_fg"

    ["ui.cursor.insert"]
    fg = "base0A"
    modifiers = ["underlined"]

    ["ui.cursor.match"]
    fg = "base0A"
    modifiers = ["underlined"]

    ["ui.cursor.select"]
    bg = "base05"
    fg = "base02"

    ["ui.cursorline.primary"]
    bg = "base01"
    fg = "base05"

    ["ui.gutter"]
    bg = "base00"

    ["ui.help"]
    bg = "base01"
    fg = "base06"

    ["ui.linenr"]
    bg = "base00"
    fg = "base03"

    ["ui.linenr.selected"]
    bg = "base01"
    fg = "base04"
    modifiers = ["bold"]

    ["ui.menu"]
    bg = "base01"
    fg = "base05"

    ["ui.menu.scroll"]
    bg = "base01"
    fg = "base03"

    ["ui.menu.selected"]
    bg = "base04"
    fg = "base01"

    ["ui.popup"]
    bg = "base01"

    ["ui.selection"]
    bg = "base02"

    ["ui.selection.primary"]
    bg = "base0C"
    fg = "base02"

    ["ui.statusline"]
    bg = "base02"
    fg = "base0B"

    ["ui.statusline.inactive"]
    bg = "base01"
    fg = "base02"

    ["ui.statusline.insert"]
    bg = "base0B"
    fg = "base00"

    ["ui.statusline.normal"]
    bg = "base04"
    fg = "base00"

    ["ui.statusline.select"]
    bg = "base0E"
    fg = "base00"

    ["ui.virtual.indent-guide"]
    fg = "base03"

    ["ui.virtual.inlay-hint"]
    fg = "base03"

    ["ui.virtual.jump-label"]
    bg = "base05"
    fg = "base02"

    ["ui.virtual.ruler"]
    bg = "base01"

    ["ui.virtual.whitespace"]
    fg = "base01"

    ["ui.window"]
    bg = "base01"
  '';
}
