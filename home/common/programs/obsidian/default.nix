{
  pkgs,
  ...
}:
{
  home.packages = [
    pkgs.obsidian

    # non closed source
    pkgs.logseq
  ];

  home.persistence = {
    "/persist" = {
      directories = [
        ".config/obsidian"
      ];
    };
  };
}
