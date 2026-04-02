{
  config,
  pkgs,
  inputs,
  outputs,
  ...
}:
{
  users = {
    mutableUsers = false;
    users.root.hashedPasswordFile = "/persist/home/secrets/passwd-root";

    users.xendak = {
      uid = 1000;
      isNormalUser = true;
      shell = pkgs.nushell;
      extraGroups = [
        "audio"
        "video"
        "input"
        "wheel"
        "networkmanager"
        "ollama"
        "open-webui"
        "podman"
      ];
      hashedPasswordFile = "/persist/home/secrets/passwd-xendak";
      packages = [ pkgs.home-manager ];
    };
  };

  home-manager = {
    users.xendak = import ../../home/xendak.nix;
    useUserPackages = true;
    useGlobalPkgs = true;
    extraSpecialArgs = {
      inherit inputs outputs;
      host = config.networking.hostName;
    };
    backupFileExtension = "hm-backup";
    overwriteBackup = true;
  };
}
