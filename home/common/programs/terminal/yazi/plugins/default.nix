{ pkgs, lib, ... }:
let
  pluginBinds = [
    # toml
    ''
      [[mgr.prepend_keymap]]
      on   = ["f"]
      run  = "plugin smart-filter"
      desc = "Iteractively go to directory"

      [[mgr.prepend_keymap]]
      on   = ["Y", "y"]
      run  = "plugin copy-file-contents content"
      desc = "Copy file(s) content only"

      [[mgr.prepend_keymap]]
      on   = ["Y",  "a"]
      run  = "plugin copy-file-contents formatted"
      desc = "Copy file(s) content only in md format" 

      [[mgr.prepend_keymap]]
      on   = [";"]
      run  = "plugin quickshell"
      desc = "Quickshell for hovered or selected items"

      # augment-commands entries
      [[mgr.prepend_keymap]]
      on = "<S-Enter>"
      run = "plugin augment-command -- open --interactive"
      desc = "Enter the child directory, or open the file"

      [[mgr.prepend_keymap]]
      on = "<Enter>"
      run = "plugin augment-command -- enter"
      desc = "Enter the child directory, or open the file"

      [[mgr.prepend_keymap]]
      on = "o"
      run = "plugin augment-command -- open --interactive"
      desc = "Enter the child directory, or open the file"

      [[mgr.prepend_keymap]]
      on = "O"
      run = "plugin augment-command -- open"
      desc = "Enter the child directory, or open the file"

      [[mgr.prepend_keymap]]
      on = "A"
      run = "plugin augment-command -- archive"
      desc = "Add files to an archive"

      [[mgr.prepend_keymap]]
      on = "+"
      run = "plugin augment-command -- create"
      desc = "Create a file or directory"

    ''
  ];
in
{
  #pkgs needed to run my plugins
  home.packages = [
    pkgs.exiftool
    pkgs.glow
    pkgs.eza
    pkgs.bat
  ];

  _module.args.pluginBinds = lib.concatStringSep "\n" pluginBinds;
  # deprecated for now
  home.file."config/yazi/plugins/smart-enter/main.lua".source = ./smart-enter.lua;

  home.file.".config/yazi/plugins/quickshell.yazi/main.lua".source = ./quickshell.lua;

  # https://github.com/hankertrix/augment-command.yazi/
  home.file.".config/yazi/plugins/augment-command.yazi/main.lua".source = ./augment-command.lua;

  # Copy with format or just normal copy (for ai's)
  home.file.".config/yazi/plugins/copy-file-contents.yazi/main.lua".source = ./copy_contents.lua;

  # pipe into anything.. make preview work for anything
  home.file.".config/yazi/plugins/piper.yazi/main.lua".source = ./piper.lua;

  # faster filter for entering directories
  home.file.".config/yazi/plugins/smart-filter.yazi/main.lua".source = ./smart-filter.lua;

  home.file.".config/yazi/yazi.toml".source =
    pkgs.writeText "yazi.toml"
      # toml
      ''
        [[plugin.append_previewers]]
        name = "*"
        run  = 'piper -- hexyl --border=none --terminal-width=$w "$1"'

        [[plugin.prepend_previewers]]
        name = "*.md"
        run  = 'piper -- CLICOLOR_FORCE=1 glow -w=$w "$1"'

        [[plugin.prepend_previewers]]
        name = "*/"
        run  = 'piper -- eza -TL=3 --color=always --icons=always --group-directories-first --no-quotes "$1"'

        [[plugin.preprend_previwers]]
        mime = "audio/*"
        run = 'piper -- exiftool -q -q -S -Title -SortName -TitleSort -TitleSortOrder -Artist -SortArtist -ArtistSort -PerformerSortOrder -Album -SortAlbum -AlbumSort -AlbumSortOrder -AlbumArtist -SortAlbumArtist -AlbumArtistSort -AlbumArtistSortOrder -Genre -TrackNumber -Year -Duration -SampleRate-AudioSampleRate -AudioBitrate -AvgBitrate -Channels -AudioChannels  "$1"'

        [[plugin.prepend_previewers]]
        name = "*.tar*"
        run  = 'piper --format=url -- tar tf "$1"'

        [[plugin.prepend_previewers]]
        name = "*.csv"
        run  = 'piper -- bat -p --color=always "$1"'

        [[opener.emacs]]
        run = "${pkgs.emacs-pgtk}/bin/emacsclient -c -r -- %s"
        block = false
        orphan = true
        desc = "Open in Emacs"

        [[opener.extract]]
        run = "ya pub augmented-extract --list %s"
        desc = "Extract here"

        [[opener.hexdump]]
        run = "hexyl --border=none -- %s | bat -p --paging always --pager $PAGER"
        block = true
        desc = "Open in hexview"

        [[opener.open]]
        run = "handlr open %s"
        desc = "Open"

        [[opener.reveal]]
        run = "handlr open %d"
        desc = "Reveal"

        [open]
        rules = [
        	# Folder
        	{ url = "*/", use = [ "edit", "open", "reveal" ] },
        	# Text
        	{ mime = "text/*", use = [ "edit", "open", "hexdump" ] },
        	# Image
        	{ mime = "image/*", use = [ "open", "reveal" ] },
        	# Media
        	{ mime = "{audio,video}/*", use = [ "play", "reveal" ] },
        	# Archive
        	{ mime = "application/{zip,rar,7z*,tar,gzip,xz,zstd,bzip*,lzma,compress,archive,cpio,arj,xar,ms-cab*}", use = [ "extract", "reveal" ] },
        	# JSON
        	{ mime = "application/{json,ndjson}", use = [ "edit", "hexdump", "reveal" ] },
        	{ mime = "*/javascript", use = [ "edit", "reveal" ] },
        	# Empty file
        	{ mime = "inode/empty", use = [ "edit", "reveal", "hexdump" ] },
        	# Virtual file system
        	{ mime = "vfs/{absent,stale}", use = "download" },
        	# Fallback
        	{ url = "*", use = [ "open", "hexdump", "reveal" ] },
        ]

      '';
}
