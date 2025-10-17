{ paletteSet, ... }:
let
  p = paletteSet.palette;
in
{
  "yazi/theme.toml" = # toml
    ''
      # vim:fileencoding=utf-8:foldmethod=marker
      # https://github.com/poperigby/gruvbox-dark-yazi/blob/main/theme.toml

      # : Manager {{{

      [mgr]
      cwd = { fg = "${p.base0D}" }

      # Hovered
      hovered         = { fg = "${p.base00}", bg = "${p.base0D}" }
      preview_hovered = { underline = true }

      # Find
      find_keyword  = { fg = "${p.base0B}", italic = true }
      find_position = { fg = "${p.base09}", bg = "reset", italic = true }

      # Marker
      marker_selected = { fg = "${p.base0B}", bg = "${p.base02}" }
      marker_copied   = { fg = "${p.base0C}", bg = "${p.base02}" }
      marker_cut      = { fg = "${p.base08}", bg = "${p.base02}" }

      # Tab
      tab_active   = { fg = "${p.base00}", bg = "${p.base03}" }
      tab_inactive = { fg = "${p.base04}", bg = "${p.base01}" }
      tab_width    = 1

      # Border
      border_symbol = "│"
      border_style  = { fg = "${p.base03}" }

      # : }}}


      # : Status {{{

      [status]
      separator_open  = ""
      separator_close = ""
      separator_style = { fg = "${p.base01}", bg = "${p.base01}" }

      # Progress
      progress_label  = { fg = "${p.base06}", bold = true }
      progress_normal = { fg = "${p.base03}", bg = "${p.base01}" }
      progress_error  = { fg = "${p.base08}", bg = "${p.base01}" }

      # Permissions
      permissions_t = { fg = "${p.base03}" }
      permissions_r = { fg = "${p.base0B}" }
      permissions_w = { fg = "${p.base08}" }
      permissions_x = { fg = "${p.base0C}" }
      permissions_s = { fg = "${p.base02}" }

      # : }}}


      # : mode {{{

      [mode]
      normal_main = { fg = "${p.base00}", bg = "${p.base0D}", bold = true }
      normal_alt  = { fg = "${p.base00}", bg = "${p.base0D}", bold = true }

      select_main = { fg = "${p.base00}", bg = "${p.base0B}", bold = true }
      select_alt  = { fg = "${p.base00}", bg = "${p.base0B}", bold = true }

      unset_main  = { fg = "${p.base00}", bg = "${p.base0E}", bold = true }
      unset_alt   = { fg = "${p.base00}", bg = "${p.base0E}", bold = true }

      # : }}}


      # : Input {{{

      [input]
      border   = { fg = "${p.base03}" }
      title    = {}
      value    = { fg = "${p.base05}" }
      selected = { reversed = true }

      # : }}}


      # : Select {{{

      [select]
      border   = { fg = "${p.base03}" }
      active   = { fg = "${p.base09}" }
      inactive = { fg = "${p.base04}" }

      # : }}}


      # : Tasks {{{

      [tasks]
      border  = { fg = "${p.base03}" }
      title   = {}
      hovered = { underline = true }

      # : }}}


      # : Which {{{

      [which]
      mask            = { bg = "${p.base02}" }
      cand            = { fg = "${p.base0D}" }
      rest            = { fg = "${p.base03}" }
      desc            = { fg = "${p.base09}" }
      separator       = "  "
      separator_style = { fg = "${p.base03}" }

      # : }}}


      # : Help {{{

      [help]
      on      = { fg = "${p.base09}" }
      exec    = { fg = "${p.base0D}" }
      desc    = { fg = "${p.base04}" }
      hovered = { bg = "${p.base01}", bold = true }
      footer  = { fg = "${p.base00}", bg = "${p.base04}" }

      # : }}}


      # : File-specific styles {{{

      [filetype]
      rules = [
          # Images
          { mime = "image/*", fg = "${p.base0E}" },

          # Videos
          { mime = "video/*", fg = "${p.base0C}" },
          { mime = "audio/*", fg = "${p.base0C}" },

          # Archives
          { mime = "application/zip",           fg = "${p.base09}" },
          { mime = "application/gzip",          fg = "${p.base09}" },
          { mime = "application/x-tar",         fg = "${p.base09}" },
          { mime = "application/x-bzip",        fg = "${p.base09}" },
          { mime = "application/x-bzip2",       fg = "${p.base09}" },
          { mime = "application/x-7z-compressed", fg = "${p.base09}" },
          { mime = "application/x-rar",         fg = "${p.base09}" },

          # Fallback
          { name = "*", fg = "${p.base05}" },
          { name = "*/", fg = "${p.base0D}", bold = true }
      ]

      # : }}}
    '';
}
