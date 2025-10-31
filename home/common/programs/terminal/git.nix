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
      ".zig-cache/"
      "zig-out/"
      ".direnv/"
    ];

  };
}
