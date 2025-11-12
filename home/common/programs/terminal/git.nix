{ pkgs, ... }:
{
  home.packages = [
    pkgs.gh
    pkgs.lazygit
    pkgs.gemini-cli
  ];

  home.persistence."/persist".directories = [ ".config/gh" ];

  programs.lazygit.enable = true;

  programs.delta = {
    enable = true;
    enableGitIntegration = true;
    options = {
      dark = true;
      side-by-side = true;
      line-numbers = true;
      hyperlinks = true;
      syntax-theme = "base16";
      navigate = true;
    };
  };

  programs.git = {
    enable = true;

    settings = {
      credential.helper = "${pkgs.git.override { withLibsecret = true; }}/bin/git-credential-libsecret";
      diff.colorMoved = "default";
      merge.conflictstyle = "diff3";
      user.email = "108767275+xendak@users.noreply.github.com";
      user.name = "Rafael Grossi";
    };

    ignores = [
      "*.swp"
      "*~"
      "node_modules"
      ".zig-cache/"
      "zig-out/"
      ".direnv/"
    ];

  };
}
