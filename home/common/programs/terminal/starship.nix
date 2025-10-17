{
  ...
}:
{
  programs.starship = {
    enable = true;
  };
  xdg.configFile."starship.toml".text = ''
    "$schema" = 'https://starship.rs/config-schema.json'

    format = """
    [╭─](white)\
    [](color_green)\
    $nix_shell\
    $username\
    [](bg:color_purple fg:color_green)\
    $directory\
    [](fg:color_purple bg:color_red)\
    $git_branch\
    $git_status\
    [](fg:color_red bg:color_blue)\
    $c\
    $rust\
    $golang\
    $nodejs\
    $zig\
    $php\
    $java\
    $kotlin\
    $haskell\
    $python\
    [](fg:color_blue bg:color_bg3)\
    $docker_context\
    $conda\
    [](fg:color_bg3 bg:color_bg1)\
    $cmd_duration\
    [ ](fg:color_bg1)\
    $line_break\
    [╰─](white)$character"""

    palette = 'gruvbox_dark'

    [palettes.gruvbox_dark]
    color_bg1 = "white"
    color_bg3 = "bright-blue"
    color_blue = "cyan"
    color_red = "red"
    color_green = "green"
    color_orange = "bright-red"
    color_purple = "bright-yellow"
    color_aqua = "blue"
    color_yellow = "bright-cyan"

    [os]
    disabled = false
    style = "bg:color_green fg:black"
    [nix_shell]
    format = "[$symbol]($style)"
    symbol = " "
    disabled=false
    heuristic=true
    style = "bg:color_green fg:black"


    [os.symbols]
    Windows = "󰍲"
    NixOS = " "
    Ubuntu = "󰕈"
    SUSE = ""
    Raspbian = "󰐿"
    Mint = "󰣭"
    Macos = "󰀵"
    Manjaro = ""
    Linux = "󰌽"
    Gentoo = "󰣨"
    Fedora = "󰣛"
    Alpine = ""
    Amazon = ""
    Android = ""
    Arch = "󰣇"
    Artix = "󰣇"
    CentOS = ""
    Debian = "󰣚"
    Redhat = "󱄛"
    RedHatEnterprise = "󱄛"

    [username]
    show_always = true
    style_user = "bg:color_green fg:black"
    style_root = "bg:color_green fg:black"
    format = '[ $user ]($style)'

    [directory]
    style = "fg:black bg:color_purple"
    format = "[ $path ]($style)"
    truncation_length = 3
    truncation_symbol = "…/"

    [directory.substitutions]
    "Documents" = "󰈙 "
    "Downloads" = " "
    "Music" = "󰝚 "
    "Pictures" = " "
    "Developer" = "󰲋 "

    [git_branch]
    symbol = ""
    style = "bg:color_red"
    format = '[[ $symbol $branch ](fg:black bg:color_red)]($style)'

    [git_status]
    style = "bg:color_red"
    format = '[[($all_status$ahead_behind )](fg:black bg:color_red)]($style)'

    [nodejs]
    symbol = ""
    style = "bg:color_blue"
    format = '[[ $symbol( $version) ](fg:black bg:color_blue)]($style)'

    [c]
    symbol = " "
    style = "bg:color_blue"
    format = '[[ $symbol( $version) ](fg:black bg:color_blue)]($style)'

    [rust]
    symbol = ""
    style = "bg:color_blue"
    format = '[[ $symbol( $version) ](fg:black bg:color_blue)]($style)'

    [golang]
    symbol = ""
    style = "bg:color_blue"
    format = '[[ $symbol( $version) ](fg:black bg:color_blue)]($style)'

    [php]
    symbol = ""
    style = "bg:color_blue"
    format = '[[ $symbol( $version) ](fg:black bg:color_blue)]($style)'

    [java]
    symbol = " "
    style = "bg:color_blue"
    format = '[[ $symbol( $version) ](fg:black bg:color_blue)]($style)'

    [kotlin]
    symbol = ""
    style = "bg:color_blue"
    format = '[[ $symbol( $version) ](fg:black bg:color_blue)]($style)'

    [haskell]
    symbol = ""
    style = "bg:color_blue"
    format = '[[ $symbol( $version) ](fg:black bg:color_blue)]($style)'

    [zig]
    symbol = ""
    style = "bg:color_blue"
    format = '[[ $symbol( $version) ](fg:black bg:color_blue)]($style)'

    [python]
    symbol = ""
    style = "bg:color_blue"
    format = '[[ $symbol( $version) ](fg:black bg:color_blue)]($style)'

    [docker_context]
    symbol = ""
    style = "bg:color_bg3"
    format = '[[ $symbol( $context) ](fg:#83a598 bg:color_bg3)]($style)'

    [conda]
    style = "bg:color_bg3"
    format = '[[ $symbol( $environment) ](fg:#83a598 bg:color_bg3)]($style)'

    [cmd_duration]
    disabled = false
    style = "bg:color_bg1"
    format = '[[  $duration ](fg:black bg:color_bg1)]($style)'

    [line_break]
    disabled = false

    [character]
    disabled = false
    success_symbol = '[ᐉ ](bold fg:orange)'
    error_symbol = '[ᐉ ](bold fg:aqua)'
    vimcmd_symbol = '[ᐉ ](bold fg:blue)'
    vimcmd_replace_one_symbol = '[ᐉ ](bold fg:yellow)'
    vimcmd_replace_symbol = '[ᐉ ](bold fg:yellow)'
    vimcmd_visual_symbol = '[ᐉ ](bold fg:purple)'
  '';
}
