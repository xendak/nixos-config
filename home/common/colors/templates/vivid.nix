{ paletteSet, lib, ... }:
let
  p = paletteSet.palette;
  hex = str: lib.removePrefix "#" str;
in
{
  "vivid/themes/current.yml" = ''
    colors:
      background:   '${hex p.background}'
      foreground:   '${hex p.on_background}'
      surface:      '${hex p.surface}'
      on_surface:   '${hex p.on_surface}'
      surface_low:  '${hex p.surface_container_low}'
      surface_high: '${hex p.surface_container_high}'

      primary:      '${hex p.primary}'   # green 
      secondary:    '${hex p.secondary}' # yellow
      tertiary:     '${hex p.tertiary}'  # blue
      on_primary:   '${hex p.on_primary}'
      on_secondary: '${hex p.on_secondary}'
      on_tertiary:  '${hex p.on_tertiary}'

      green:        '${hex p.green}'
      yellow:       '${hex p.yellow}'
      blue:         '${hex p.blue}'
      gray:         '${hex p.gray}'
      red:          '${hex p.red}'
      magenta:      '${hex p.magenta}'
      cyan:         '${hex p.cyan}'
      orange:       '${hex p.orange}'

      dim:          '${hex p.outline}'

    core:
      normal_text:
        foreground: dim

      reset_to_normal:
        background: background
        foreground: foreground
        font-style: regular

    # File Types
      regular_file:
        foreground: foreground

      directory:
        foreground: blue
        font-style: bold

      multi_hard_link:
        foreground: blue
        font-style: underline

      symlink:
        foreground: blue

      broken_symlink:
        foreground: red

      missing_symlink_target:
        background: red
        foreground: on_surface
        font-style: bold

      fifo:
        foreground: dim
        font-style:
          - bold
          - underline

      character_device:
        foreground: dim

      block_device:
        foreground: dim
        font-style: underline

      door:
        foreground: dim
        font-style: italic

      socket:
        foreground: dim
        font-style: bold

    # File Permissions
      executable_file:
        foreground: green
        font-style: bold

      file_with_capability:
        foreground: yellow
        font-style:
          - bold
          - underline

      setuid:
        foreground: on_surface
        background: surface_high
        font-style:
          - bold
          - underline

      setgid:
        foreground: on_surface
        background: surface_low
        font-style:
          - bold
          - underline

      sticky:
        background: dim
        foreground: foreground
        font-style: underline

      other_writable:
        background: dim
        foreground: foreground
        font-style: bold

      sticky_other_writable:
        background: dim
        foreground: foreground
        font-style:
          - bold
          - underline

    # Document Types
    archives:
      foreground: orange
      font-style: bold

    executable:
      foreground: green
      font-style: bold

    markup:
      foreground: cyan
      web:
        foreground: magenta

    media:
      foreground: cyan
      fonts:
        foreground: magenta

    office:
      foreground: yellow

    programming:
      source:
        foreground: yellow
      tooling:
        foreground: gray

    text:
      foreground: foreground

    unimportant:
      foreground: dim
  '';
}
