{ ... }:
{
  nix = {
    buildMachines = [
      {
        hostName = "Snow";
        sshUser = "xendak";
        sshKey = "/etc/ssh/ssh_host_ed25519_key";
        system = "x86_64-linux";
        protocol = "ssh-ng";
        maxJobs = 20;
        speedFactor = 3;
        supportedFeatures = [
          "nixos-test"
          "benchmark"
          "big-parallel"
          "kvm"
        ];
      }
    ];
    distributedBuilds = true;
    settings = {
      builders-use-substitutes = true;
      fallback = true;
      warn-dirty = false;
      connect-timeout = 10;
      substituters = [
        "https://cache.nixos.org/"
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
  };
}
