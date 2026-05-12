{ host, ... }:
let
  exitNodeHost = "Snow";
in
{
  services.tailscale = {
    enable = true;
    useRoutingFeatures = "both";

    extraUpFlags =
      if host == exitNodeHost then
        [ "--advertise-exit-node" ]
      else
        [
          "--exit-node=${exitNodeHost}"
          "--exit-node-allow-lan-access"
        ];
  };

  # Standard port for Tailscale's WireGuard traffic
  networking.firewall.allowedUDPPorts = [ 41641 ];
  # maybe bypass school thing
  networking.firewall.allowedTCPPorts = [ 443 ];

  environment.persistence."/persist".directories = [
    "/var/lib/tailscale"
  ];
}
