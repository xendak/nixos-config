{
  paletteSet,
  ...
}:
let
  m = paletteSet.palette;
in
{
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

      # : Manager [[[

      [mgr]
      cwd = { fg = "${m.on_surface}" }

      # Find
      find_keyword  = { fg = "${m.error}", bold = true, italic = true, underline = true }
      find_position = { fg = "${m.error}", bold = true, italic = true }

      # Marker
      marker_copied   = { fg = "${m.tertiary_fixed}", bg = "${m.on_tertiary_fixed}" }
      marker_cut      = { fg = "${m.tertiary_fixed}", bg = "${m.on_tertiary_fixed}" }
      marker_marked   = { fg = "${m.error}", bg = "${m.error}" }
      marker_selected = { fg = "${m.tertiary}", bg = "${m.tertiary}" }

      # Count
      count_copied   = { fg = "${m.on_tertiary_fixed}", bg = "${m.tertiary_fixed}" }
      count_cut      = { fg = "${m.on_tertiary_fixed}", bg = "${m.tertiary_fixed}" }
      count_selected = { fg = "${m.on_primary}", bg = "${m.tertiary}" }

      # Border
      border_symbol = "│"
      border_style  = { fg = "${m.surface_tint}" }

      # : ]]]


      # : Indicator [[[

      [indicator]
      padding = { open = "█", close = "█" }

      # : ]]]


      # : Tabs [[[

      [tabs]
      active    = { fg = "${m.surface_container_low}", bold = true, bg = "${m.secondary}" }
      inactive  = { fg = "${m.secondary}", bg = "${m.surface_container_low}" }
      # sep_inner = { open = "[", close = "]" }

      # : ]]]


      # : Mode [[[

      [mode]
      # Mode
      normal_main = { bg = "${m.primary}", fg = "${m.on_primary}", bold = true }
      normal_alt  = { bg = "${m.secondary}", fg = "${m.on_secondary}" }

      # Select mode
      select_main = { bg = "${m.secondary}", fg = "${m.on_secondary}", bold = true }
      select_alt  = { bg = "${m.tertiary}", fg = "${m.on_tertiary}" }

      # Unset mode
      unset_main = { bg = "${m.tertiary}", fg = "${m.on_tertiary}", bold = true }
      unset_alt  = { bg = "${m.surface_variant}", fg = "${m.on_surface_variant}" }

      # : ]]]


      # : Status [[[

      [status]
      # Permissions
      perm_type  = { fg = "${m.yellow}" }
      perm_write = { fg = "${m.green}" }
      perm_read  = { fg = "${m.blue}" }
      perm_exec  = { fg = "${m.red}" }
      perm_sep   = { fg = "${m.magenta}" }

      # Progress
      progress_label  = { bold = true }
      progress_normal = { fg = "${m.primary}", bg = "${m.surface_container_high}" }
      progress_error  = { fg = "${m.error}", bg = "${m.surface_container}" }

      # : ]]]


      # : Which [[[

      [which]
      cols = 3
      mask = { bg = "${m.surface_bright}" }
      cand = { fg = "${m.primary}" }
      rest = { fg = "${m.on_primary}" }
      desc = { fg = "${m.on_surface}" }
      separator = " ▶ "
      separator_style = { fg = "${m.on_surface}" }

      # : ]]]


      # : Notify [[[

      [notify]
      title_info  = { fg = "${m.tertiary}" }
      title_warn  = { fg = "${m.primary}" }
      title_error = { fg = "${m.error}" }

      # : ]]]


      # : Picker [[[

      [pick]
      border = { fg = "${m.primary}" }
      active = { fg = "${m.tertiary}", bold = true }
      inactive = {}

      # : ]]]


      # : Input [[[

      [input]
      border = { fg = "${m.primary}" }
      value  = { fg = "${m.on_surface}" }

      # : ]]]


      # : Completion [[[

      [cmp]
      border = { fg = "${m.primary}", bg = "${m.on_primary}" }

      # : ]]]


      # : Tasks [[[

      [tasks]
      border  = { fg = "${m.primary}" }
      title   = {}
      hovered = { fg = "${m.tertiary_fixed}", underline = true }

      # : ]]]


      # : Help [[[

      [help]
      on     = { fg = "${m.on_surface}" }
      run    = { fg = "${m.on_surface}" }
      footer = { fg = "${m.on_secondary}", bg = "${m.secondary}" }

      # : ]]]


    '';
}
