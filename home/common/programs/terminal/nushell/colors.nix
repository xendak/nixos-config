# TODO EVENTUALLY
{ colorscheme }:
let c = colorscheme.pallette;
{ 

let base16_theme = {
    separator: $base03
    leading_trailing_space_bg: $base04
    header: $accent
    date: $base0e
    filesize: $base0d
    row_index: $base0c
    bool: $base08
    int: $base0b
    duration: $base08
    range: $base08
    float: $base08
    string: $base04
    nothing: $base08
    binary: $base08
    cellpath: $base08
    hints: dark_gray

    shape_garbage: { fg: "#FFFFFF" bg: "#FF0000" attr: b }
    shape_bool: $base0d
    shape_int: { fg: $base0e attr: b }
    shape_float: { fg: $base0e attr: b }
    shape_range: { fg: $base0a attr: b }
    shape_internalcall: { fg: $base0c attr: b }
    shape_external: $base0c
    shape_externalarg: { fg: $base0b attr: b }
    shape_literal: $base0d
    shape_operator: $base0a
    shape_signature: { fg: $base0b attr: b }
    shape_string: $base0b
    shape_filepath: $base0d
    shape_globpattern: { fg: $base0d attr: b }
    shape_variable: $base0e
    shape_flag: { fg: $base0d attr: b }
    shape_custom: { attr: b }
}

def hex-to-ansi-fg [hex_color: string] {
    let r = ($hex_color | str substring 1..3 | into int -r 16)
    let g = ($hex_color | str substring 3..5 | into int -r 16)
    let b = ($hex_color | str substring 5..7 | into int -r 16)
    $"38;2;($r);($g);($b)"
}

let ls_colors_theme = {
    di: $"1;(hex-to-ansi-fg $accent)"  # Directory: Bold Blue
    ln: $"1;(hex-to-ansi-fg $base10)"  # Symbolic Link: Bold Aqua
    ex: (hex-to-ansi-fg $base10)       # Executable: Bold Green
    fi: (hex-to-ansi-fg $base05)       # Regular File: Default Foreground

    # Special files
    pi: (hex-to-ansi-fg $base0a)      # Pipe: Yellow
    so: $"1;(hex-to-ansi-fg $base0e)"  # Socket: Bold Purple
    or: $"1;(hex-to-ansi-fg $base08)"  # Orphaned Symlink: Bold Red

    # Common archive file extensions
    "*.tar.gz": (hex-to-ansi-fg $base08)
    "*.zip": (hex-to-ansi-fg $base08)
    "*.rar": (hex-to-ansi-fg $base08)
    "*.gz": (hex-to-ansi-fg $base08)
    "*.tgz": (hex-to-ansi-fg $base08)

    # Common image file extensions
    "*.png": (hex-to-ansi-fg $base0e)
    "*.jpg": (hex-to-ansi-fg $base0e)
    "*.jpeg": (hex-to-ansi-fg $base0e)
    "*.gif": (hex-to-ansi-fg $base0e)
    "*.webp": (hex-to-ansi-fg $base0e)
}
}

# $env.LS_COLORS = $ls_colors_theme | to nuon

# $env.config.color_config = $base16_theme
