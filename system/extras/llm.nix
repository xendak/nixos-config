{
  inputs,
  lib,
  pkgs,
  ...
}:
let
  llama-cpp-custom = pkgs.llama-cpp.override {
    vulkanSupport = true;
    rocmSupport = false;
  };

  rocmEnv = {
    HSA_OVERRIDE_GFX_VERSION = "10.3.0";
    ROC_ENABLE_PRE_VEGA = "1";
    HIP_VISIBLE_DEVICES = "0";
  };
in
{
  virtualisation.podman.enable = true;

  environment.systemPackages = [
    pkgs.llama-swap
    llama-cpp-custom
  ];

  networking.firewall.interfaces."tailscale0".allowedTCPPorts = [
    8080
    11434
  ];

  systemd.services.llama-swap = {
    description = "Llama-swap Model Proxy";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    environment = rocmEnv;
    path = [
      llama-cpp-custom
      pkgs.coreutils
    ];
    serviceConfig = {
      ExecStart = ''
        ${pkgs.llama-swap}/bin/llama-swap \
          -listen 0.0.0.0:11434 \
          -config /local/nixos/data/AI/models/llama-swap.yaml
      '';
      Restart = "always";
      User = "xendak";
    };
  };

  services.open-webui = {
    enable = true;
    # FIX(xendak): openwebui 0.9.5 is broken ;)
    package =
      let
        pkgs-owui = import inputs.nixpkgs-owui {
          system = pkgs.stdenv.hostPlatform.system;
          config.allowUnfree = true;
        };
      in
      pkgs-owui.open-webui;
    host = "0.0.0.0";
    port = 8080;
    stateDir = "/local/nixos/data/AI/open-webui";
    environment = {
      ANONYMIZED_TELEMETRY = "False";
      DO_NOT_TRACK = "True";
      SCARF_NO_ANALYTICS = "True";
      WEBUI_AUTH = "False";

      # OpenAI-compat pointing at llama-swap
      OPENAI_API_BASE_URL = "http://127.0.0.1:11434/v1";
      OPENAI_API_KEY = "dummy";
      ENABLE_OLLAMA_API = "False";

      FRONTEND_BUILD_DIR = "/local/nixos/data/AI/open-webui/build";
      DATA_DIR = "/local/nixos/data/AI/open-webui/data";
      STATIC_DIR = "/local/nixos/data/AI/open-webui/static";
      PIPELINES_URL = "http://127.0.0.1:9099";
      ENABLE_PIPELINES = "True";
    };
  };

  systemd.services.open-webui.serviceConfig = {
    User = "xendak";
    Group = "users";
    DynamicUser = lib.mkForce false;
    StateDirectory = lib.mkForce "";
  };

  virtualisation.oci-containers.containers."open-webui-pipelines" = {
    image = "ghcr.io/open-webui/pipelines:main";
    user = "1000:100";
    ports = [ "9099:9099" ];
    volumes = [ "/local/nixos/data/AI/pipelines:/app/pipelines" ];
    environment = {
      PIPELINES_URL = "http://127.0.0.1:9099";
      PIPELINES_API_KEY = "0p3n-w3bu!";
    };
    extraOptions = [ "--network=host" ];
  };

  systemd.services."podman-open-webui-pipelines" = {
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    serviceConfig = {
      Restart = "on-failure";
      RestartSec = "10s";
    };
  };
}
