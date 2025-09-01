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
      directories = [
        "Games/Lutris-Prefix"
        ".config/lutris"
        ".local/share/lutris"
        ".local/cache/lutris"
      ];
    };
  };
}
