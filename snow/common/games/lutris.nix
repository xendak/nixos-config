{ pkgs, lib, ... }: {
  home.packages = with pkgs; [ 
    (lutris.override {
      extraPkgs = pkgs: with pkgs; [ vulkan-loader vulkan-tools curl nghttp2 xdelta mangohud winetricks gamescope];
      }
    )
  ];

  home.persistence = {
    "/persist/snow/flakes/" = {
      allowOther = true;
      directories = [ 
        "Games/Lutris"
        "Games/Wine" 
        "Games/Wine-Prefix" 
        ".config/lutris" 
        ".local/share/lutris" 
        ".local/cache/lutris"
      ];
    };
  };
}
