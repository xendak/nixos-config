{ ... }:
{
  services.tailscale = {
    enable = true;
    useRoutingFeatures = "both";

    extraUpFlags = "--advertise-exit-node";
  };

  # Standard port for Tailscale's WireGuard traffic
  networking.firewall.allowedUDPPorts = [ 41641 ];
  # maybe bypass school thing
  networking.firewall.allowedTCPPorts = [ 443 ];

  environment.persistence."/persist".directories = [
    "/var/lib/tailscale"
  ];
}
