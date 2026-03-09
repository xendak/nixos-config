{ host, ... }:
{
  imports = [
    (
      if host == "Dew" then
        ./drops/home.nix
      else if host == "Snow" then
        ./flakes/home.nix
      else if host == "wsl" then
        ./nixos/home.nix
      else
        throw "Unknown host: ${host}"
    )
  ];
}
