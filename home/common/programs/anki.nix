{
  config,
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [ anki-bin ];

  # EVENTUALLY:
  # https://github.com/LilleAila/dotfiles/blob/main/pkgs/anki.nix

  home.persistence = {
    "/persist" = {
      directories = [
        ".local/share/Anki"
        ".local/share/Anki2"
      ];
      allowOther = true;
    };
  };
}
