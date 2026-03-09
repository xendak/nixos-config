{
  outputs,
  lib,
  config,
  ...
}:
let
  inherit (config.networking) hostName;
  hosts = outputs.nixosConfigurations;
  pubKey = host: ./${host}/ssh_host_ed25519_key.pub;

  hasOptinPersistence =
    config.environment ? persistence && config.environment.persistence ? "/persist";
in
{

  services.openssh = {
    enable = true;
    settings = {
      # Harden
      PasswordAuthentication = false;
      PermitRootLogin = "no";
      # Automatically remove stale sockets
      StreamLocalBindUnlink = "yes";
      # Allow forwarding ports to everywhere
      GatewayPorts = "clientspecified";
      AcceptEnv = [ "WAYLAND_DISPLAY" ];
      X11Forwarding = true;
    };

    hostKeys = [
      {
        path = "${lib.optionalString hasOptinPersistence "/persist"}/etc/ssh/ssh_host_ed25519_key";
        type = "ed25519";
      }
    ];
  };

  programs.ssh = {
    startAgent = true;
    # Each hosts public key
    knownHosts = builtins.mapAttrs (name: _: {
      publicKeyFile = pubKey name;
      extraHostNames = [
        "${name}.local"
      ]
      ++ lib.optional (name == hostName) "localhost";
    }) hosts;
  };

  # Passwordless sudo when SSH'ing with keys
  security.pam.sshAgentAuth = {
    enable = true;
    authorizedKeysFiles = [ "/etc/ssh/authorized_keys.d/%u" ];
  };
}
