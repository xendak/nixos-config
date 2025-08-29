{
  config,
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    (lutris.override {
      extraPkgs =
        pkgs: with pkgs; [
          vulkan-loader
          vulkan-tools
          curl
          nghttp2
          xdelta
          mangohud
          winetricks
          gamescope
        ];
    })
  ];

  home.persistence = {
    "/persist" = {
      allowOther = true;
      directories = [
        "Games/Wine"
        "Games/Wine-Prefix"
        "Games/Lutris-Prefix"
        ".config/lutris"
        ".local/share/lutris"
        ".local/cache/lutris"
      ];
    };
  };
}
