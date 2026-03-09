{ paletteSet, lib, ... }:
let
  m = paletteSet.palette;
  hex = str: lib.removePrefix "#" str;
in
{
  "vivid/themes/current.yml" = ''
    colors:
      # ── M3 surface tiers ─────────────────────────────────────────────────
      # (nord0-3 equivalents: dark background steps)
      bg:           '${hex m.background}'
      surface_low:  '${hex m.surface_container_low}'
      surface:      '${hex m.surface_container}'
      surface_high: '${hex m.surface_container_high}'

      # ── M3 foreground tiers ───────────────────────────────────────────────
      # (nord4-6 equivalents: light text steps)
      fg:           '${hex m.on_surface}'
      fg_bright:    '${hex m.on_background}'
      fg_brightest: '${hex m.inverse_on_surface}'

      # ── M3 accent tiers ───────────────────────────────────────────────────
      pri_cont:     '${hex m.on_primary_container}'    # nord7  teal-ish
      pri:          '${hex m.primary}'                 # nord8  primary
      sec:          '${hex m.secondary}'               # nord9  secondary
      sec_dark:     '${hex m.on_secondary_container}'  # nord10 darker secondary

      # ── M3 semantic colors ────────────────────────────────────────────────
      error:        '${hex m.error}'                   # nord11 red
      tertiary:     '${hex m.tertiary}'                # nord12 orange-ish
      ter_cont:     '${hex m.on_tertiary_container}'   # nord13 yellow-ish
      green:        '${hex m.primary}'                 # nord14 green → primary
      magenta:      '${hex m.on_secondary_container}'  # nord15 magenta

      dim_fg:       '${hex m.outline}'                 # dim-foreground

    core:
      normal_text:
        foreground: dim_fg
      reset_to_normal:
        background: bg
        foreground: fg
        font-style: regular

    # ── File Types ───────────────────────────────────────────────────────────
      regular_file:
        foreground: fg

      directory:
        foreground: sec_dark
        font-style: bold

      multi_hard_link:
        foreground: pri
        font-style: underline

      symlink:
        foreground: pri

      broken_symlink:
        foreground: error

      missing_symlink_target:
        # BG HERE #
        background: surface_high
        foreground: error
        font-style: bold

      fifo:
        foreground: pri_cont
        font-style:
          - bold
          - underline

      character_device:
        foreground: ter_cont

      block_device:
        foreground: ter_cont
        font-style: underline

      door:
        foreground: ter_cont
        font-style: italic

      socket:
        foreground: ter_cont
        font-style: bold

    # ── File Permissions ──────────────────────────────────────────────────────
      executable_file:
        foreground: pri_cont
        font-style: bold

      file_with_capability:
        foreground: fg
        font-style:
          - bold
          - underline

      setuid:
        foreground: fg
        font-style:
          - bold
          - underline

      setgid:
        foreground: fg
        font-style:
          - bold
          - underline

      sticky:
        # BG HERE #
        background: surface_high
        foreground: fg_bright
        font-style: underline

      other_writable:
        # BG HERE #
        background: surface
        foreground: fg_bright
        font-style: bold

      sticky_other_writable:
        background: surface_low
        foreground: fg_bright
        font-style:
          - bold
          - underline

    # ── Document Types ────────────────────────────────────────────────────────
    archives:
      foreground: fg_bright
      font-style: bold

    executable:
      foreground: pri_cont
      font-style: bold

    markup:
      foreground: fg_brightest
      web:
        foreground: fg

    media:
      foreground: magenta
      fonts:
        foreground: fg

    office:
      foreground: green

    programming:
      source:
        foreground: pri_cont
      tooling:
        foreground: fg

    text:
      foreground: fg

    unimportant:
      foreground: surface_high
  '';
}
