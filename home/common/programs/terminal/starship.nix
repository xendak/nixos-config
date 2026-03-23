{
  ...
}:
{
  programs.starship = {
    enable = false;
  };
  xdg.configFile."starship.toml".text = ''
    "$schema" = 'https://starship.rs/config-schema.json'

    format = """
    [┌─](black)$character\
    $nix_shell\
    $username\
    $hostname\
    $directory\
    $git_branch\
    $git_status\
    $c$cpp$rust$golang$nodejs$zig$php$java$kotlin$haskell$python$odin\
    $docker_context\
    $conda\
    $cmd_duration\
    $line_break\
    [└─](black)ᐉ """

    scan_timeout = 10

    [nix_shell]
    format = '[\[[$symbol$state]($style)\]](black)[─](black)'
    symbol = ' '
    style = 'bold blue'

    [username]
    show_always = false
    style_user = 'blue'
    style_root = 'red'
    format = '[\[[$user]($style)](blue)'

    [hostname]
    ssh_only = true
    style = 'blue'
    format = '[[@](green)[$hostname]($style)\]](blue)[─](black)'

    [directory]
    style = 'blue'
    format = '[\[[$path]($style)\]](black)' 
    truncation_length = 3
    truncation_symbol = '…/'

    [git_branch]
    symbol = ' '
    style = 'red'
    format = '[─](black)[\[[$symbol$branch]($style)\]](black)'

    [git_status]
    style = 'red'
    format = '[\[](black)[$all_status$ahead_behind]($style)[\]](black)'

    [cmd_duration]
    min_time = 500
    style = 'gray'
    format = '[─](black)[\[[󱎫 $duration](gray)\]](black)'

    [character]
    format="$symbol"
    success_symbol = '[\(^.^\)](green)[─](black)'
    error_symbol = '[\(x.x\)](red)[─](black)'
    vimcmd_symbol = '[\(✧.✧\)](blue)[─](black)'

    [nodejs]
    symbol = ""
    style = "black"
    format = '[[[─\[]($style) $symbol( $version) ](green)\]]($style)'

    [c]
    symbol = " "
    style = "black"
    format = '[[[─\[]($style) $symbol( $version) ](green)\]]($style)'

    [cpp]
    symbol = " "
    style = "black"
    format = '[[[─\[]($style) $symbol( $version) ](green)\]]($style)'

    [odin]
    style = "black"
    format = '[[[─\[]($style) $symbol($version) ](green)\]]($style)'

    [rust]
    symbol = ""
    style = "black"
    format = '[[[─\[]($style) $symbol( $version) ](green)\]]($style)'

    [golang]
    symbol = ""
    style = "black"
    format = '[[[─\[]($style) $symbol( $version) ](green)\]]($style)'

    [php]
    symbol = ""
    style = "black"
    format = '[[[─\[]($style) $symbol( $version) ](green)\]]($style)'

    [java]
    symbol = " "
    style = "black"
    format = '[[[─\[]($style) $symbol( $version) ](green)\]]($style)'

    [kotlin]
    symbol = ""
    style = "black"
    format = '[[[─\[]($style) $symbol( $version) ](green)\]]($style)'

    [haskell]
    symbol = ""
    style = "black"
    format = '[[[─\[]($style) $symbol( $version) ](green)\]]($style)'

    [python]
    symbol = ""
    style = "black"
    format = '[[[─\[]($style) $symbol( $version) ](green)\]]($style)'

    [zig]
    symbol = ""
    style = "black"
    format = '[[[─\[]($style) $symbol( $version) ](green)\]]($style)'

    [docker_context]
    symbol = ""
    style = "bg:color_bg3"
    format = '[[ $symbol( $context) ](fg:#83a598 bg:color_bg3)]($style)'

    [conda]
    style = "bg:color_bg3"
    format = '[[ $symbol( $environment) ](fg:#83a598 bg:color_bg3)]($style)'
  '';
}
