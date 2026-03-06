{ paletteSet, ... }:
let
  m = paletteSet.palette;
in
{
  "yazi/some.toml" = # toml
    ''
      [which]
      mask = { bg = "${m.surface_container}" }
    '';

  "yazi/theme.toml" = # toml
    ''
      # vim:fileencoding=utf-8:foldmethod=marker
      # Material Design 3 – auto-generated
      # Mapping:
      #   cwd / navigation accent  → secondary
      #   hover (selected item)    → primary_container / on_primary_container
      #   find / search highlight  → primary
      #   markers                  → primary / secondary / error containers
      #   tabs active              → on_primary_container / primary_container
      #   borders                  → outline_variant
      #   mode badges              → on_primary / primary (normal),
      #                             on_tertiary / tertiary (select),
      #                             on_secondary / secondary (unset)

      # : Manager {{{

      [mgr]
      cwd = { fg = "${m.secondary}" }

      hovered         = { fg = "${m.on_primary_container}", bg = "${m.primary_container}" }
      preview_hovered = { underline = true }

      find_keyword  = { fg = "${m.primary}", italic = true }
      find_position = { fg = "${m.on_primary}", bg = "reset", italic = true }

      marker_selected = { fg = "${m.on_primary_container}",   bg = "${m.primary_container}" }
      marker_copied   = { fg = "${m.on_secondary_container}", bg = "${m.secondary_container}" }
      marker_cut      = { fg = "${m.on_error_container}",     bg = "${m.error_container}" }

      tab_active   = { fg = "${m.on_primary_container}", bg = "${m.primary_container}" }
      tab_inactive = { fg = "${m.on_surface_variant}",   bg = "${m.surface_container_low}" }
      tab_width    = 1

      border_symbol = "│"
      border_style  = { fg = "${m.outline_variant}" }

      # : }}}


      # : Status {{{

      [status]
      separator_open  = ""
      separator_close = ""
      separator_style = { fg = "${m.surface_container_low}", bg = "${m.surface_container_low}" }

      progress_label  = { fg = "${m.on_surface}", bold = true }
      progress_normal = { fg = "${m.outline}",    bg = "${m.surface_container_low}" }
      progress_error  = { fg = "${m.error}",      bg = "${m.surface_container_low}" }

      permissions_t = { fg = "${m.outline}" }
      permissions_r = { fg = "${m.primary}" }
      permissions_w = { fg = "${m.error}" }
      permissions_x = { fg = "${m.secondary}" }
      permissions_s = { fg = "${m.outline_variant}" }

      # : }}}


      # : Mode {{{

      [mode]
      normal_main = { fg = "${m.on_primary}",   bg = "${m.primary}",   bold = true }
      normal_alt  = { fg = "${m.on_primary}",   bg = "${m.primary}",   bold = true }

      select_main = { fg = "${m.on_tertiary}",  bg = "${m.tertiary}",  bold = true }
      select_alt  = { fg = "${m.on_tertiary}",  bg = "${m.tertiary}",  bold = true }

      unset_main  = { fg = "${m.on_secondary}", bg = "${m.secondary}", bold = true }
      unset_alt   = { fg = "${m.on_secondary}", bg = "${m.secondary}", bold = true }

      # : }}}


      # : Input {{{

      [input]
      border   = { fg = "${m.outline}" }
      title    = {}
      value    = { fg = "${m.on_surface}" }
      selected = { reversed = true }

      # : }}}


      # : Select {{{

      [select]
      border   = { fg = "${m.outline}" }
      active   = { fg = "${m.primary}" }
      inactive = { fg = "${m.on_surface_variant}" }

      # : }}}


      # : Tasks {{{

      [tasks]
      border  = { fg = "${m.outline}" }
      title   = {}
      hovered = { underline = true }

      # : }}}


      # : Which {{{

      [which]
      mask            = { bg = "${m.surface_container}" }
      cand            = { fg = "${m.secondary}" }
      rest            = { fg = "${m.outline}" }
      desc            = { fg = "${m.on_surface_variant}" }
      separator       = "  "
      separator_style = { fg = "${m.outline_variant}" }

      # : }}}


      # : Help {{{

      [help]
      on      = { fg = "${m.primary}" }
      exec    = { fg = "${m.secondary}" }
      desc    = { fg = "${m.on_surface_variant}" }
      hovered = { bg = "${m.surface_container_high}", bold = true }
      footer  = { fg = "${m.on_primary}", bg = "${m.primary}" }

      # : }}}


      # : File-specific styles {{{

      [filetype]
      rules = [
          { mime = "image/*",                     fg = "${m.on_secondary_container}" },
          { mime = "video/*",                     fg = "${m.on_primary_container}" },
          { mime = "audio/*",                     fg = "${m.on_primary_container}" },
          { mime = "application/zip",             fg = "${m.tertiary}" },
          { mime = "application/gzip",            fg = "${m.tertiary}" },
          { mime = "application/x-tar",           fg = "${m.tertiary}" },
          { mime = "application/x-bzip",          fg = "${m.tertiary}" },
          { mime = "application/x-bzip2",         fg = "${m.tertiary}" },
          { mime = "application/x-7z-compressed", fg = "${m.tertiary}" },
          { mime = "application/x-rar",           fg = "${m.tertiary}" },
          { name = "*",                           fg = "${m.on_surface}" },
          { name = "*/",                          fg = "${m.secondary}", bold = true },
      ]

      # : }}}
    '';
}
