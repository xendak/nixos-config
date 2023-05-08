{pkgs, ...}:
{
  imports = [
    ./kitty.nix 
    ./fish.nix 
    ./git.nix
    ./nvim
  ];
}

