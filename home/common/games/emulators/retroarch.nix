{
  pkgs,
  config,
  ...
}:
{
  home.packages = [

    (pkgs.retroarch.withCores (
      cores:
      with cores;
      [
        ppsspp
        snes9x
        dolphin
        mupen64plus
        desmume
        mgba
      ]
      ++ (
        if pkgs.stdenv.hostPlatform.system == "x86_64-linux" then
          [
            # stuff that doesn't happen on ARM
            pcsx2
          ]
        else
          [ ]
      )
    ))
  ];

  home.persistence = {
    "/persist" = {
      directories = [
        ".local/share/retroarch"
        ".config/retroarch"
        "Games/Emulators/RetroArch"
      ];
    };
  };
}
