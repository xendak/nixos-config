{ pkgs, ... }:
{
  home.packages = [
    pkgs.gh
    pkgs.lazygit
    pkgs.gemini-cli
  ];

  home.persistence."/persist".directories = [ ".config/gh" ];

  programs.lazygit = {
    enable = true;
    # settings = {
    #   disableStartupPopups = true;
    #   promptToReturnFromSubprocess = false;
    #   git.paging.externalDiffCommand = "difft --color=always --syntax-highlight=on --tab-width=2";
    #   gui = {
    #     language = "en";
    #     nerdFontsVersion = "3";
    #     filterMode = "fuzzy";
    #     refresher = {
    #       refreshInterval = 120;
    #       fetchInverval = 120;
    #     };
    #     os = {
    #       open = "xdg-open {{filename}} >/dev/null";
    #       openLink = "xdg-open {{link}} >/dev/null";
    #       copyToClipboardCmd = ''printf "\033]52;c;$(printf {{text}} | base64)\a" > /dev/tty '';
    #       editPreset = "helix (hx)";
    #       editInTerminal = true;
    #     };
    #   };
    #   customCommands = [
    #     {
    #       key = "C";
    #       command = "git cz";
    #       context = "files";
    #       loadingText = "opening commitizen commit tool";
    #       output = "terminal";
    #     }
    #     {
    #       key = "E";
    #       description = "Add empty commit";
    #       context = "commits";
    #       command = "git commit --allow-empty -m 'empty commit'";
    #       loadingText = "Committing empty commit...";
    #     }
    #     {
    #       key = "f";
    #       command = "git difftool -y {{.SelectedLocalCommit.Sha}} -- {{.SelectedCommitFile.Name}}";
    #       context = "commitFiles";
    #       description = "Compare (difftool) with local copy";
    #     }
    #     {
    #       key = "<c-a>";
    #       description = "Pick AI commit";
    #       command = ''sh lazygit-ai-commit "gemini -m gemini-2.5-pro -p"'';
    #       context = "files";
    #       output = "terminal";
    #     }
    #     {
    #       key = "<c-A>";
    #       description = "Pick AI commit";
    #       command = ''sh lazygit-ai-commit "gemini -m gemini-2.5-flash -p"'';
    #       context = "files";
    #       output = "terminal";
    #     }
    #   ];
    # };
  };

  programs.git = {
    enable = true;

    extraConfig = {
      credential.helper = "${pkgs.git.override { withLibsecret = true; }}/bin/git-credential-libsecret";
      diff.colorMoved = "default";
      merge.conflictstyle = "diff3";
    };

    ignores = [
      "*.swp"
      ".direnv"
    ];

    userEmail = "108767275+xendak@users.noreply.github.com";
    userName = "Rafael Grossi";
  };
}
