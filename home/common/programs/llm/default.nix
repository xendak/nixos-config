{
  pkgs,
  inputs,
  host,
  ...
}:
let
  baseURL = if host == "Snow" then "http://localhost:11434/v1" else "http://snow:11434/v1";
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
    pkgs.opencode

    # trying out
    pkgs.pi-coding-agent
    pkgs.jan
  ];

  xdg.configFile."opencode/config.json".text = builtins.toJSON {
    "$schema" = "https://opencode.ai/config.json";
    provider = {
      local-swap = {
        npm = "@ai-sdk/openai-compatible";
        name = "Llama-Swap (Local)";
        options = {
          inherit baseURL;
          apiKey = "dummy";
        };
        models = {
          "Qwen:instruct" = {
            name = "Qwen Instruct";
          };
          "Qwen:thinking" = {
            name = "Qwen Thinking";
          };
          "Qwen:thinking-coding" = {
            name = "Qwen Thinking (Coding)";
          };
          "Qwen:instruct-reasoning" = {
            name = "Qwen Instruct + Reasoning";
          };
        };
      };
    };
  };

  home.persistence."/persist" = {
    directories = [
      ".config/com.jeffser.Alpaca"
      ".local/share/com.jeffser.Alpaca"
      ".local/cache/com.jeffser.Alpaca"

      ".config/opencode"
      ".local/share/opencode"
    ];
  };
}
