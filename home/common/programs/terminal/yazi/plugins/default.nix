{ pkgs, ... }:
{
  #pkgs needed to run my plugins
  home.packages = [
    pkgs.exiftool
    pkgs.glow
    pkgs.eza
    pkgs.bat
  ];

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
      '';
}
