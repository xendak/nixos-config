{ pkgs, config, ... }:
let 
  c = config.colorscheme.palette;
in
{
  programs.starship = {
    enable = true;
  };
  xdg.configFile."starship.toml".text = ''
    "$schema" = 'https://starship.rs/config-schema.json'

    format = """
    [](color_green)\
    $os\
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
    $line_break$character"""

    palette = 'gruvbox_dark'

    [palettes.gruvbox_dark]
    color_fg0 = '#${c.base00}'
    color_bg1 = '#${c.base05}'
    color_bg3 = '#${c.base04}'
    color_blue = '#${c.base0C}'
    color_red = '#${c.base08}'
    color_green = '#${c.base0B}'
    color_orange = '#${c.base09}'
    color_purple = '#${c.base0E}'
    color_aqua = '#${c.base0D}'
    color_yellow = '#${c.base0F}'

    [os]
    disabled = false
    style = "bg:color_green fg:color_fg0"

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
    style_user = "bg:color_green fg:color_fg0"
    style_root = "bg:color_green fg:color_fg0"
    format = '[ $user ]($style)'

    [directory]
    style = "fg:color_fg0 bg:color_purple"
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
    format = '[[ $symbol $branch ](fg:color_fg0 bg:color_red)]($style)'

    [git_status]
    style = "bg:color_red"
    format = '[[($all_status$ahead_behind )](fg:color_fg0 bg:color_red)]($style)'

    [nodejs]
    symbol = ""
    style = "bg:color_blue"
    format = '[[ $symbol( $version) ](fg:color_fg0 bg:color_blue)]($style)'

    [c]
    symbol = " "
    style = "bg:color_blue"
    format = '[[ $symbol( $version) ](fg:color_fg0 bg:color_blue)]($style)'

    [rust]
    symbol = ""
    style = "bg:color_blue"
    format = '[[ $symbol( $version) ](fg:color_fg0 bg:color_blue)]($style)'

    [golang]
    symbol = ""
    style = "bg:color_blue"
    format = '[[ $symbol( $version) ](fg:color_fg0 bg:color_blue)]($style)'

    [php]
    symbol = ""
    style = "bg:color_blue"
    format = '[[ $symbol( $version) ](fg:color_fg0 bg:color_blue)]($style)'

    [java]
    symbol = " "
    style = "bg:color_blue"
    format = '[[ $symbol( $version) ](fg:color_fg0 bg:color_blue)]($style)'

    [kotlin]
    symbol = ""
    style = "bg:color_blue"
    format = '[[ $symbol( $version) ](fg:color_fg0 bg:color_blue)]($style)'

    [haskell]
    symbol = ""
    style = "bg:color_blue"
    format = '[[ $symbol( $version) ](fg:color_fg0 bg:color_blue)]($style)'

    [python]
    symbol = ""
    style = "bg:color_blue"
    format = '[[ $symbol( $version) ](fg:color_fg0 bg:color_blue)]($style)'

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
    format = '[[  $duration ](fg:color_fg0 bg:color_bg1)]($style)'

    [line_break]
    disabled = false

    [character]
    disabled = false
    success_symbol = '[ᐉ ](bold fg:color_orange)'
    error_symbol = '[ᐉ ](bold fg:color_aqua)'
    vimcmd_symbol = '[ᐉ ](bold fg:color_blue)'
    vimcmd_replace_one_symbol = '[ᐉ ](bold fg:color_yellow)'
    vimcmd_replace_symbol = '[ᐉ ](bold fg:color_yellow)'
    vimcmd_visual_symbol = '[ᐉ ](bold fg:color_purple)'

  '';
}
