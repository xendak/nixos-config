{ config, ... }:
{
  services.tailscale = {
    enable = true;
    useRoutingFeatures = "both";

    extraUpFlags =
      if config.networking.hostName == "Snow" then [ "--advertise-exit-node" ] else [ "--accept-routes" ];
  };

  # Standard port for Tailscale's WireGuard traffic
  networking.firewall.allowedUDPPorts = [ 41641 ];
  # maybe bypass school thing
  networking.firewall.allowedTCPPorts = [ 443 ];

  environment.persistence."/persist".directories = [
    "/var/lib/tailscale"
  ];
}
