{ config, ... }:
{
  programs = {

    carapace = {
      enable = true;
      enableNushellIntegration = true;
    };
    starship.enable = true;
    starship.enableNushellIntegration = true;
    yazi.enableNushellIntegration = true;
    zoxide.enableNushellIntegration = true;

    nushell = {
      enable = true;
      shellAliases = {
        ll = "ls";
        lg = "lazygit";
        y = "yazi";
      };
      extraConfig = # nu
        ''
          let carapace_completer = {|spans|
            carapace $spans.0 nushell ...$spans | from json
           }
          $env.PATH = ($env.PATH | 
            split row (char esep) |
            prepend /home/''${config.home.username}/Flake/bin |
            append /usr/bin/env
          )

          $env.config = {
            buffer_editor = hx,
            edit_mode: vi,
            show_banner: false,
            shell_integration: {
              osc2: false,
              osc7: true,
              osc8: false,
              osc133: true,
              osc1337: true,
              osc633: false,
              reset_application_mode: true,
            },
            completions: {
              quick: true,
              partial: true,
              algorithm: "fuzzy",
            },
            history: {
              sync_on_enter: true,
            },
          }
        '';
    };
  };
}
