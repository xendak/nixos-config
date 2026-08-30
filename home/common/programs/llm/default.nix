{
  pkgs,
  inputs,
  host,
  ...
}:
let
  baseURL = if host == "Snow" then "http://localhost:11434/v1" else "http://snow:11434/v1";
  pi-wrapped = pkgs.symlinkJoin {
    name = "pi-coding-agent-wrapped";
    paths = [ pkgs.pi-coding-agent ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/pi \
        --run 'export PI_CODING_AGENT_DIR="''${PI_CODING_AGENT_DIR:-$HOME/.config/picode}"'
    '';
  };
in
{
  imports = [
    inputs.agenix.homeManagerModules.default
  ];

  # set -gx GEMINI_API_KEY (cat /run/agenix.d/1/gemini-api-key)
  programs.fish.loginShellInit = ''
    if test -f  "$HOME/.ssh/gemini"
      eval set -gx GEMINI_API_KEY (cat "$HOME/.ssh/gemini")
      rm "$HOME/.ssh/gemini"
    end
  '';
  home.packages = [
    # pkgs.alpaca
    # pkgs.opencode
    # pkgs.pi-coding-agent
    pi-wrapped
  ];

  home.persistence."/persist" = {
    directories = [
      ".config/picode"
    ];
  };
}
