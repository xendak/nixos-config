{
  pkgs,
  inputs,
  config,
  ...
}:
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
}
