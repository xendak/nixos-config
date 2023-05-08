{ pkgs, config, ... }:
{
  home.packages = [
    pkgs.gh
  ];

  programs.git = {
    enable = true;
    
    extraConfig = {
      credential.helper = "${pkgs.git.override { withLibsecret = true; } }/bin/git-credential-libsecret";
      diff.colorMoved = "default";
      merge.conflictstyle = "diff3";
    };

    ignores = ["*.swp" ".direnv"];

    userEmail = "rg.grossi@outlook.com";
    userName = "Rafael Grossi";
  };
}
