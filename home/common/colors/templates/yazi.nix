{
  paletteSet,
  ...
}:
let
  m = paletteSet.palette;
  src = m.primary;
  arc = m.secondary;
  doc = m.on_secondary_container;
  bin = m.tertiary;
  dim = m.surface_container_high;
  dev = m.on_tertiary_container;
  lnk = m.on_primary_container;
  err = m.error;
  # wht = m.inverse_on_surface;
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
        # ── Special types ─────────────────────────────────────────────────
        { url = "*",  is = "block",  fg = "${dev}", bg = "${m.surface_container}", bold = true },
        { url = "*",  is = "char",   fg = "${dev}", bg = "${m.surface_container}", bold = true },
        { url = "*/",                fg = "${lnk}", bold = true },
        { url = "*",  is = "exec",   fg = "${bin}", bold = true },
        { url = "*",  is = "link",   fg = "${src}", bold = true },
        { url = "*",  is = "orphan", fg = "${err}", bg = "${m.surface_container_high}", bold = true },
        { url = "*",  is = "fifo",   fg = "${dev}", bg = "${m.surface_container}", bold = true },
        { url = "*",  is = "sock",   fg = "${doc}", bg = "${m.surface_container}", bold = true },
        { url = "*",  is = "sticky" },
        # ── Extensions ────────────────────────────────────────────────────
        { url = "*.1",   fg = "${doc}" },
        { url = "*.a",   fg = "${bin}" },
        { url = "*.c",   fg = "${src}" },
        { url = "*.d",   fg = "${src}" },
        { url = "*.h",   fg = "${src}" },
        { url = "*.m",   fg = "${src}" },
        { url = "*.o",   fg = "${dim}" },
        { url = "*.p",   fg = "${src}" },
        { url = "*.r",   fg = "${src}" },
        { url = "*.t",   fg = "${src}" },
        { url = "*.v",   fg = "${src}" },
        { url = "*.z",   fg = "${arc}", bold = true },
        { url = "*.7z",  fg = "${arc}", bold = true },
        { url = "*.ai",  fg = "${doc}" },
        { url = "*.as",  fg = "${src}" },
        { url = "*.bc",  fg = "${dim}" },
        { url = "*.bz",  fg = "${arc}", bold = true },
        { url = "*.cc",  fg = "${src}" },
        { url = "*.cp",  fg = "${src}" },
        { url = "*.cr",  fg = "${src}" },
        { url = "*.cs",  fg = "${src}" },
        { url = "*.db",  fg = "${arc}", bold = true },
        { url = "*.di",  fg = "${src}" },
        { url = "*.el",  fg = "${src}" },
        { url = "*.ex",  fg = "${src}" },
        { url = "*.fs",  fg = "${src}" },
        { url = "*.go",  fg = "${src}" },
        { url = "*.gv",  fg = "${src}" },
        { url = "*.gz",  fg = "${arc}", bold = true },
        { url = "*.ha",  fg = "${src}" },
        { url = "*.hh",  fg = "${src}" },
        { url = "*.hi",  fg = "${dim}" },
        { url = "*.hs",  fg = "${src}" },
        { url = "*.jl",  fg = "${src}" },
        { url = "*.js",  fg = "${src}" },
        { url = "*.ko",  fg = "${bin}" },
        { url = "*.kt",  fg = "${src}" },
        { url = "*.la",  fg = "${dim}" },
        { url = "*.ll",  fg = "${src}" },
        { url = "*.lo",  fg = "${dim}" },
        { url = "*.ma",  fg = "${doc}" },
        { url = "*.mb",  fg = "${doc}" },
        { url = "*.md",  fg = "${doc}" },
        { url = "*.mk",  fg = "${src}" },
        { url = "*.ml",  fg = "${src}" },
        { url = "*.mn",  fg = "${src}" },
        { url = "*.nb",  fg = "${src}" },
        { url = "*.nu",  fg = "${src}" },
        { url = "*.pl",  fg = "${src}" },
        { url = "*.pm",  fg = "${src}" },
        { url = "*.pp",  fg = "${src}" },
        { url = "*.ps",  fg = "${doc}" },
        { url = "*.py",  fg = "${src}" },
        { url = "*.rb",  fg = "${src}" },
        { url = "*.rm",  fg = "${doc}" },
        { url = "*.rs",  fg = "${src}" },
        { url = "*.sh",  fg = "${src}" },
        { url = "*.so",  fg = "${bin}" },
        { url = "*.td",  fg = "${src}" },
        { url = "*.ts",  fg = "${src}" },
        { url = "*.ui",  fg = "${doc}" },
        { url = "*.vb",  fg = "${src}" },
        { url = "*.wv",  fg = "${doc}" },
        { url = "*.xz",  fg = "${arc}", bold = true },
        { url = "*FAQ",  fg = "${doc}" },
        { url = "*.3ds", fg = "${doc}" },
        { url = "*.3fr", fg = "${doc}" },
        { url = "*.3mf", fg = "${doc}" },
        { url = "*.adb", fg = "${src}" },
        { url = "*.ads", fg = "${src}" },
        { url = "*.aif", fg = "${doc}" },
        { url = "*.amf", fg = "${doc}" },
        { url = "*.ape", fg = "${doc}" },
        { url = "*.apk", fg = "${arc}", bold = true },
        { url = "*.ari", fg = "${doc}" },
        { url = "*.arj", fg = "${arc}", bold = true },
        { url = "*.arw", fg = "${doc}" },
        { url = "*.asa", fg = "${src}" },
        { url = "*.asm", fg = "${src}" },
        { url = "*.aux", fg = "${dim}" },
        { url = "*.avi", fg = "${doc}" },
        { url = "*.awk", fg = "${src}" },
        { url = "*.bag", fg = "${arc}", bold = true },
        { url = "*.bak", fg = "${dim}" },
        { url = "*.bat", fg = "${bin}" },
        { url = "*.bay", fg = "${doc}" },
        { url = "*.bbl", fg = "${dim}" },
        { url = "*.bcf", fg = "${dim}" },
        { url = "*.bib", fg = "${doc}" },
        { url = "*.bin", fg = "${arc}", bold = true },
        { url = "*.blg", fg = "${dim}" },
        { url = "*.bmp", fg = "${doc}" },
        { url = "*.bsh", fg = "${src}" },
        { url = "*.bst", fg = "${doc}" },
        { url = "*.bz2", fg = "${arc}", bold = true },
        { url = "*.c++", fg = "${src}" },
        { url = "*.cap", fg = "${doc}" },
        { url = "*.cfg", fg = "${doc}" },
        { url = "*.cgi", fg = "${src}" },
        { url = "*.clj", fg = "${src}" },
        { url = "*.com", fg = "${bin}" },
        { url = "*.cpp", fg = "${src}" },
        { url = "*.cr2", fg = "${doc}" },
        { url = "*.cr3", fg = "${doc}" },
        { url = "*.crw", fg = "${doc}" },
        { url = "*.css", fg = "${src}" },
        { url = "*.csv", fg = "${doc}" },
        { url = "*.csx", fg = "${src}" },
        { url = "*.cxx", fg = "${src}" },
        { url = "*.dae", fg = "${doc}" },
        { url = "*.dcr", fg = "${doc}" },
        { url = "*.dcs", fg = "${doc}" },
        { url = "*.deb", fg = "${arc}", bold = true },
        { url = "*.def", fg = "${src}" },
        { url = "*.dll", fg = "${bin}" },
        { url = "*.dmg", fg = "${arc}", bold = true },
        { url = "*.dng", fg = "${doc}" },
        { url = "*.doc", fg = "${doc}" },
        { url = "*.dot", fg = "${src}" },
        { url = "*.dox", fg = "${src}" },
        { url = "*.dpr", fg = "${src}" },
        { url = "*.drf", fg = "${doc}" },
        { url = "*.dxf", fg = "${doc}" },
        { url = "*.eip", fg = "${doc}" },
        { url = "*.elc", fg = "${src}" },
        { url = "*.elm", fg = "${src}" },
        { url = "*.epp", fg = "${src}" },
        { url = "*.eps", fg = "${doc}" },
        { url = "*.erf", fg = "${doc}" },
        { url = "*.erl", fg = "${src}" },
        { url = "*.exe", fg = "${bin}" },
        { url = "*.exr", fg = "${doc}" },
        { url = "*.exs", fg = "${src}" },
        { url = "*.fbx", fg = "${doc}" },
        { url = "*.fff", fg = "${doc}" },
        { url = "*.fls", fg = "${dim}" },
        { url = "*.flv", fg = "${doc}" },
        { url = "*.fnt", fg = "${doc}" },
        { url = "*.fon", fg = "${doc}" },
        { url = "*.fsi", fg = "${src}" },
        { url = "*.fsx", fg = "${src}" },
        { url = "*.gif", fg = "${doc}" },
        { url = "*.git", fg = "${dim}" },
        { url = "*.gpr", fg = "${doc}" },
        { url = "*.gvy", fg = "${src}" },
        { url = "*.h++", fg = "${src}" },
        { url = "*.hda", fg = "${doc}" },
        { url = "*.hip", fg = "${doc}" },
        { url = "*.hpp", fg = "${src}" },
        { url = "*.htc", fg = "${src}" },
        { url = "*.htm", fg = "${doc}" },
        { url = "*.hxx", fg = "${src}" },
        { url = "*.ico", fg = "${doc}" },
        { url = "*.ics", fg = "${doc}" },
        { url = "*.idx", fg = "${dim}" },
        { url = "*.igs", fg = "${doc}" },
        { url = "*.iiq", fg = "${doc}" },
        { url = "*.ilg", fg = "${dim}" },
        { url = "*.img", fg = "${arc}", bold = true },
        { url = "*.inc", fg = "${src}" },
        { url = "*.ind", fg = "${dim}" },
        { url = "*.ini", fg = "${doc}" },
        { url = "*.inl", fg = "${src}" },
        { url = "*.ino", fg = "${src}" },
        { url = "*.ipp", fg = "${src}" },
        { url = "*.iso", fg = "${arc}", bold = true },
        { url = "*.jar", fg = "${arc}", bold = true },
        { url = "*.jpg", fg = "${doc}" },
        { url = "*.jsx", fg = "${src}" },
        { url = "*.jxl", fg = "${doc}" },
        { url = "*.k25", fg = "${doc}" },
        { url = "*.kdc", fg = "${doc}" },
        { url = "*.kex", fg = "${doc}" },
        { url = "*.kra", fg = "${doc}" },
        { url = "*.kts", fg = "${src}" },
        { url = "*.log", fg = "${dim}" },
        { url = "*.ltx", fg = "${src}" },
        { url = "*.lua", fg = "${src}" },
        { url = "*.m3u", fg = "${doc}" },
        { url = "*.m4a", fg = "${doc}" },
        { url = "*.m4v", fg = "${doc}" },
        { url = "*.mdc", fg = "${doc}" },
        { url = "*.mef", fg = "${doc}" },
        { url = "*.mid", fg = "${doc}" },
        { url = "*.mir", fg = "${src}" },
        { url = "*.mkv", fg = "${doc}" },
        { url = "*.mli", fg = "${src}" },
        { url = "*.mos", fg = "${doc}" },
        { url = "*.mov", fg = "${doc}" },
        { url = "*.mp3", fg = "${doc}" },
        { url = "*.mp4", fg = "${doc}" },
        { url = "*.mpg", fg = "${doc}" },
        { url = "*.mrw", fg = "${doc}" },
        { url = "*.msi", fg = "${arc}", bold = true },
        { url = "*.mtl", fg = "${doc}" },
        { url = "*.nef", fg = "${doc}" },
        { url = "*.nim", fg = "${src}" },
        { url = "*.nix", fg = "${doc}" },
        { url = "*.nrw", fg = "${doc}" },
        { url = "*.obj", fg = "${doc}" },
        { url = "*.obm", fg = "${doc}" },
        { url = "*.odp", fg = "${doc}" },
        { url = "*.ods", fg = "${doc}" },
        { url = "*.odt", fg = "${doc}" },
        { url = "*.ogg", fg = "${doc}" },
        { url = "*.ogv", fg = "${doc}" },
        { url = "*.orf", fg = "${doc}" },
        { url = "*.org", fg = "${doc}" },
        { url = "*.otf", fg = "${doc}" },
        { url = "*.otl", fg = "${doc}" },
        { url = "*.out", fg = "${dim}" },
        { url = "*.pas", fg = "${src}" },
        { url = "*.pbm", fg = "${doc}" },
        { url = "*.pcx", fg = "${doc}" },
        { url = "*.pdf", fg = "${doc}" },
        { url = "*.pef", fg = "${doc}" },
        { url = "*.pgm", fg = "${doc}" },
        { url = "*.php", fg = "${src}" },
        { url = "*.pid", fg = "${dim}" },
        { url = "*.pkg", fg = "${arc}", bold = true },
        { url = "*.png", fg = "${doc}" },
        { url = "*.pod", fg = "${src}" },
        { url = "*.ppm", fg = "${doc}" },
        { url = "*.pps", fg = "${doc}" },
        { url = "*.ppt", fg = "${doc}" },
        { url = "*.pro", fg = "${src}" },
        { url = "*.ps1", fg = "${src}" },
        { url = "*.psd", fg = "${doc}" },
        { url = "*.ptx", fg = "${doc}" },
        { url = "*.pxn", fg = "${doc}" },
        { url = "*.pyc", fg = "${dim}" },
        { url = "*.pyd", fg = "${dim}" },
        { url = "*.pyo", fg = "${dim}" },
        { url = "*.qoi", fg = "${doc}" },
        { url = "*.r3d", fg = "${doc}" },
        { url = "*.raf", fg = "${doc}" },
        { url = "*.rar", fg = "${arc}", bold = true },
        { url = "*.raw", fg = "${doc}" },
        { url = "*.rpm", fg = "${arc}", bold = true },
        { url = "*.rst", fg = "${doc}" },
        { url = "*.rtf", fg = "${doc}" },
        { url = "*.rw2", fg = "${doc}" },
        { url = "*.rwl", fg = "${doc}" },
        { url = "*.rwz", fg = "${doc}" },
        { url = "*.sbt", fg = "${src}" },
        { url = "*.sql", fg = "${src}" },
        { url = "*.sr2", fg = "${doc}" },
        { url = "*.srf", fg = "${doc}" },
        { url = "*.srw", fg = "${doc}" },
        { url = "*.stl", fg = "${doc}" },
        { url = "*.stp", fg = "${doc}" },
        { url = "*.sty", fg = "${dim}" },
        { url = "*.svg", fg = "${doc}" },
        { url = "*.swf", fg = "${doc}" },
        { url = "*.swp", fg = "${dim}" },
        { url = "*.sxi", fg = "${doc}" },
        { url = "*.sxw", fg = "${doc}" },
        { url = "*.tar", fg = "${arc}", bold = true },
        { url = "*.tbz", fg = "${arc}", bold = true },
        { url = "*.tcl", fg = "${src}" },
        { url = "*.tex", fg = "${src}" },
        { url = "*.tga", fg = "${doc}" },
        { url = "*.tgz", fg = "${arc}", bold = true },
        { url = "*.tif", fg = "${doc}" },
        { url = "*.tml", fg = "${doc}" },
        { url = "*.tmp", fg = "${dim}" },
        { url = "*.toc", fg = "${dim}" },
        { url = "*.tsx", fg = "${src}" },
        { url = "*.ttf", fg = "${doc}" },
        { url = "*.txt", fg = "${doc}" },
        { url = "*.typ", fg = "${doc}" },
        { url = "*.usd", fg = "${doc}" },
        { url = "*.vcd", fg = "${arc}", bold = true },
        { url = "*.vim", fg = "${src}" },
        { url = "*.vob", fg = "${doc}" },
        { url = "*.vsh", fg = "${src}" },
        { url = "*.wav", fg = "${doc}" },
        { url = "*.wma", fg = "${doc}" },
        { url = "*.wmv", fg = "${doc}" },
        { url = "*.wrl", fg = "${doc}" },
        { url = "*.x3d", fg = "${doc}" },
        { url = "*.x3f", fg = "${doc}" },
        { url = "*.xlr", fg = "${doc}" },
        { url = "*.xls", fg = "${doc}" },
        { url = "*.xml", fg = "${doc}" },
        { url = "*.xmp", fg = "${doc}" },
        { url = "*.xpm", fg = "${doc}" },
        { url = "*.xvf", fg = "${doc}" },
        { url = "*.yml", fg = "${doc}" },
        { url = "*.zig", fg = "${src}" },
        { url = "*.zip", fg = "${arc}", bold = true },
        { url = "*.zsh", fg = "${src}" },
        { url = "*.zst", fg = "${arc}", bold = true },
        { url = "*TODO",             fg = "${doc}", bold = true },
        { url = "*hgrc",             fg = "${src}" },
        { url = "*.avif",            fg = "${doc}" },
        { url = "*.bash",            fg = "${src}" },
        { url = "*.braw",            fg = "${doc}" },
        { url = "*.conf",            fg = "${doc}" },
        { url = "*.dart",            fg = "${src}" },
        { url = "*.data",            fg = "${doc}" },
        { url = "*.diff",            fg = "${src}" },
        { url = "*.docx",            fg = "${doc}" },
        { url = "*.epub",            fg = "${doc}" },
        { url = "*.fish",            fg = "${src}" },
        { url = "*.flac",            fg = "${doc}" },
        { url = "*.h264",            fg = "${doc}" },
        { url = "*.hack",            fg = "${src}" },
        { url = "*.heif",            fg = "${doc}" },
        { url = "*.hgrc",            fg = "${src}" },
        { url = "*.html",            fg = "${doc}" },
        { url = "*.iges",            fg = "${doc}" },
        { url = "*.info",            fg = "${doc}" },
        { url = "*.java",            fg = "${src}" },
        { url = "*.jpeg",            fg = "${doc}" },
        { url = "*.json",            fg = "${doc}" },
        { url = "*.less",            fg = "${src}" },
        { url = "*.lisp",            fg = "${src}" },
        { url = "*.lock",            fg = "${dim}" },
        { url = "*.make",            fg = "${src}" },
        { url = "*.mojo",            fg = "${src}" },
        { url = "*.mpeg",            fg = "${doc}" },
        { url = "*.nims",            fg = "${src}" },
        { url = "*.opus",            fg = "${doc}" },
        { url = "*.orig",            fg = "${dim}" },
        { url = "*.pptx",            fg = "${doc}" },
        { url = "*.prql",            fg = "${src}" },
        { url = "*.psd1",            fg = "${src}" },
        { url = "*.psm1",            fg = "${src}" },
        { url = "*.purs",            fg = "${src}" },
        { url = "*.raku",            fg = "${src}" },
        { url = "*.rlib",            fg = "${dim}" },
        { url = "*.sass",            fg = "${src}" },
        { url = "*.scad",            fg = "${src}" },
        { url = "*.scss",            fg = "${src}" },
        { url = "*.step",            fg = "${doc}" },
        { url = "*.tbz2",            fg = "${arc}", bold = true },
        { url = "*.tiff",            fg = "${doc}" },
        { url = "*.toml",            fg = "${doc}" },
        { url = "*.usda",            fg = "${doc}" },
        { url = "*.usdc",            fg = "${doc}" },
        { url = "*.usdz",            fg = "${doc}" },
        { url = "*.webm",            fg = "${doc}" },
        { url = "*.webp",            fg = "${doc}" },
        { url = "*.woff",            fg = "${doc}" },
        { url = "*.xbps",            fg = "${arc}", bold = true },
        { url = "*.xlsx",            fg = "${doc}" },
        { url = "*.yaml",            fg = "${doc}" },
        { url = "*stdin",            fg = "${dim}" },
        { url = "*v.mod",            fg = "${src}" },
        { url = "*.blend",           fg = "${doc}" },
        { url = "*.cabal",           fg = "${src}" },
        { url = "*.cache",           fg = "${dim}" },
        { url = "*.class",           fg = "${dim}" },
        { url = "*.cmake",           fg = "${src}" },
        { url = "*.ctags",           fg = "${dim}" },
        { url = "*.dylib",           fg = "${bin}" },
        { url = "*.dyn_o",           fg = "${dim}" },
        { url = "*.gcode",           fg = "${src}" },
        { url = "*.ipynb",           fg = "${src}" },
        { url = "*.mdown",           fg = "${doc}" },
        { url = "*.patch",           fg = "${src}" },
        { url = "*.rmeta",           fg = "${dim}" },
        { url = "*.scala",           fg = "${src}" },
        { url = "*.shtml",           fg = "${doc}" },
        { url = "*.swift",           fg = "${src}" },
        { url = "*.toast",           fg = "${arc}", bold = true },
        { url = "*.woff2",           fg = "${doc}" },
        { url = "*.xhtml",           fg = "${doc}" },
        { url = "*LEGACY",           fg = "${doc}" },
        { url = "*NOTICE",           fg = "${doc}" },
        { url = "*README",           fg = "${doc}" },
        { url = "*go.mod",           fg = "${src}" },
        { url = "*go.sum",           fg = "${dim}" },
        { url = "*passwd",           fg = "${doc}" },
        { url = "*shadow",           fg = "${doc}" },
        { url = "*stderr",           fg = "${dim}" },
        { url = "*stdout",           fg = "${dim}" },
        { url = "*.bashrc",          fg = "${src}" },
        { url = "*.config",          fg = "${doc}" },
        { url = "*.dyn_hi",          fg = "${dim}" },
        { url = "*.flake8",          fg = "${src}" },
        { url = "*.gradle",          fg = "${src}" },
        { url = "*.groovy",          fg = "${src}" },
        { url = "*.ignore",          fg = "${src}" },
        { url = "*.matlab",          fg = "${src}" },
        { url = "*.nimble",          fg = "${src}" },
        { url = "*COPYING",          fg = "${doc}" },
        { url = "*INSTALL",          fg = "${doc}" },
        { url = "*LICENCE",          fg = "${doc}" },
        { url = "*LICENSE",          fg = "${doc}" },
        { url = "*TODO.md",          fg = "${doc}", bold = true },
        { url = "*VERSION",          fg = "${doc}" },
        { url = "*.alembic",         fg = "${doc}" },
        { url = "*.desktop",         fg = "${doc}" },
        { url = "*.gemspec",         fg = "${src}" },
        { url = "*.mailmap",         fg = "${src}" },
        { url = "*Doxyfile",         fg = "${src}" },
        { url = "*Makefile",         fg = "${src}" },
        { url = "*TODO.txt",         fg = "${doc}", bold = true },
        { url = "*setup.py",         fg = "${src}" },
        { url = "*.DS_Store",        fg = "${dim}" },
        { url = "*.cmake.in",        fg = "${src}" },
        { url = "*.fdignore",        fg = "${src}" },
        { url = "*.kdevelop",        fg = "${src}" },
        { url = "*.markdown",        fg = "${doc}" },
        { url = "*.rgignore",        fg = "${src}" },
        { url = "*.tfignore",        fg = "${src}" },
        { url = "*CHANGELOG",        fg = "${doc}" },
        { url = "*COPYRIGHT",        fg = "${doc}" },
        { url = "*README.md",        fg = "${doc}" },
        { url = "*bun.lockb",        fg = "${dim}" },
        { url = "*configure",        fg = "${src}" },
        { url = "*.gitconfig",       fg = "${src}" },
        { url = "*.gitignore",       fg = "${src}" },
        { url = "*.localized",       fg = "${dim}" },
        { url = "*.scons_opt",       fg = "${dim}" },
        { url = "*.timestamp",       fg = "${dim}" },
        { url = "*CODEOWNERS",       fg = "${src}" },
        { url = "*Dockerfile",       fg = "${doc}" },
        { url = "*INSTALL.md",       fg = "${doc}" },
        { url = "*README.txt",       fg = "${doc}" },
        { url = "*SConscript",       fg = "${src}" },
        { url = "*SConstruct",       fg = "${src}" },
        { url = "*.cirrus.yml",      fg = "${src}" },
        { url = "*.gitmodules",      fg = "${src}" },
        { url = "*.synctex.gz",      fg = "${dim}" },
        { url = "*.travis.yml",      fg = "${src}" },
        { url = "*INSTALL.txt",      fg = "${doc}" },
        { url = "*LICENSE-MIT",      fg = "${doc}" },
        { url = "*MANIFEST.in",      fg = "${src}" },
        { url = "*Makefile.am",      fg = "${src}" },
        { url = "*Makefile.in",      fg = "${dim}" },
        { url = "*.applescript",     fg = "${src}" },
        { url = "*.fdb_latexmk",     fg = "${dim}" },
        { url = "*.webmanifest",     fg = "${doc}" },
        { url = "*CHANGELOG.md",     fg = "${doc}" },
        { url = "*CONTRIBUTING",     fg = "${doc}" },
        { url = "*CONTRIBUTORS",     fg = "${doc}" },
        { url = "*appveyor.yml",     fg = "${src}" },
        { url = "*configure.ac",     fg = "${src}" },
        { url = "*.bash_profile",    fg = "${src}" },
        { url = "*.clang-format",    fg = "${src}" },
        { url = "*.editorconfig",    fg = "${src}" },
        { url = "*CHANGELOG.txt",    fg = "${doc}" },
        { url = "*.gitattributes",   fg = "${src}" },
        { url = "*.gitlab-ci.yml",   fg = "${src}" },
        { url = "*CMakeCache.txt",   fg = "${dim}" },
        { url = "*CMakeLists.txt",   fg = "${src}" },
        { url = "*LICENSE-APACHE",   fg = "${doc}" },
        { url = "*pyproject.toml",   fg = "${src}" },
        { url = "*CODE_OF_CONDUCT",  fg = "${doc}" },
        { url = "*CONTRIBUTING.md",  fg = "${doc}" },
        { url = "*CONTRIBUTORS.md",  fg = "${doc}" },
        { url = "*.sconsign.dblite", fg = "${dim}" },
        { url = "*CONTRIBUTING.txt", fg = "${doc}" },
        { url = "*CONTRIBUTORS.txt", fg = "${doc}" },
        { url = "*requirements.txt", fg = "${src}" },
        { url = "*package-lock.json",   fg = "${dim}" },
        { url = "*CODE_OF_CONDUCT.md",  fg = "${doc}" },
        { url = "*.CFUserTextEncoding", fg = "${dim}" },
        { url = "*CODE_OF_CONDUCT.txt", fg = "${doc}" },
        { url = "*azure-pipelines.yml", fg = "${src}" },
      ]

      # [filetype]
      # rules = [
      #     { mime = "image/*",                     fg = "${m.on_secondary_container}" },
      #     { mime = "video/*",                     fg = "${m.on_primary_container}" },
      #     { mime = "audio/*",                     fg = "${m.on_primary_container}" },
      #     { mime = "application/zip",             fg = "${m.tertiary}" },
      #     { mime = "application/gzip",            fg = "${m.tertiary}" },
      #     { mime = "application/x-tar",           fg = "${m.tertiary}" },
      #     { mime = "application/x-bzip",          fg = "${m.tertiary}" },
      #     { mime = "application/x-bzip2",         fg = "${m.tertiary}" },
      #     { mime = "application/x-7z-compressed", fg = "${m.tertiary}" },
      #     { mime = "application/x-rar",           fg = "${m.tertiary}" },
      #     { name = "*",                           fg = "${m.on_surface}" },
      #     { name = "*/",                          fg = "${m.secondary}", bold = true },
      # ]

      # : }}}
    '';
}
