{
  branch ? "mainline",
  libsForQt5,
  fetchFromGitHub,
}: let
  inherit libsForQt5 fetchFromGitHub;
in
  {
    mainline = libsForQt5.callPackage ./base.nix rec {
      pname = "yuzu-mainline";
      version = "1390";
      branchName = branch;
      src = fetchFromGitHub {
        owner = "yuzu-emu";
        repo = "yuzu-mainline";
        rev = "mainline-0-${version}";
        sha256 = "sha256-XsB5jGRGRNkmuTqTqwWy8k+iEISP39sXBuHj/Oe5YS8=";
        fetchSubmodules = true;
      };
    };
    early-access = libsForQt5.callPackage ./base.nix rec {
      pname = "yuzu-ea";
      version = "3492";
      branchName = branch;
      src = fetchFromGitHub {
        owner = "pineappleEA";
        repo = "pineapple-src";
        rev = "EA-${version}";
        sha256 = "sha256-ZKDo7+S30oMbkWrnWUssrdGPej0LAcIsUoSyCwT8aIY=";
        fetchSubmodules = true;
      };
    };
  }
  .${branch}
