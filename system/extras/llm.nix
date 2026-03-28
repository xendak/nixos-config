{ lib, pkgs, ... }:
{

  virtualisation.podman.enable = true;
  services.ollama = {
    enable = true;
    package = pkgs.ollama-rocm;

    home = "/local/nixos/data/AI/ollama";

    # loadModels = [
    #   "qwen3.5:35b"
    #   "qwen3.5:27b"
    #   "Sugoi-14B-Ultra-GGUF"
    #   "gemma3:27b"
    # ];

    environmentVariables = {
      HSA_OVERRIDE_GFX_VERSION = "10.3.0";
      ROC_ENABLE_PRE_VEGA = "1";
      HCC_AMDGPU_TARGET = "gfx1030";

      OLLAMA_MODELS = "/local/nixos/data/AI/ollama/models";
      OLLAMA_HOST = "0.0.0.0:11434";

      # 15GB
      OLLAMA_MAX_VRAM = "15000000000";
    };

    rocmOverrideGfx = "10.3.0";
  };

  systemd.services.ollama = {
    serviceConfig = {
      ReadWritePaths = [ "/local/nixos/data/AI/ollama" ];
      StateDirectory = [ "" ];
      ProtectHome = lib.mkForce false;
    };
  };

  users.users.open-webui = {
    isSystemUser = true;
    group = "open-webui";
    extraGroups = [
      "render"
      "video"
    ];
  };

  users.groups.open-webui = { };
  services.open-webui = {
    enable = true;
    package = pkgs.open-webui;

    stateDir = "/local/nixos/data/AI/open-webui";
    environment = {
      ANONYMIZED_TELEMETRY = "False";
      DO_NOT_TRACK = "True";
      SCARF_NO_ANALYTICS = "True";
      OLLAMA_API_BASE_URL = "http://127.0.0.1:11434";

      FRONTEND_BUILD_DIR = "/local/nixos/data/AI/open-webui/build";
      DATA_DIR = "/local/nixos/data/AI/open-webui/data";
      STATIC_DIR = "/local/nixos/data/AI/open-webui/static";

      PIPELINES_URL = "http://127.0.0.1:9099";
      ENABLE_PIPELINES = "True";
      WEBUI_AUTH = "False";

    };

    host = "0.0.0.0";
    port = 8080;
  };

  virtualisation.oci-containers.containers."open-webui-pipelines" = {
    image = "ghcr.io/open-webui/pipelines:main";
    ports = [ "9099:9099" ];
    volumes = [
      "/local/nixos/data/AI/pipelines:/app/pipelines"
    ];
    environment = {
      "OLLAMA_API_BASE_URL" = "http://127.0.0.1:11434";
      "PIPELINES_URL" = "http://127.0.0.1:9099";
      "PIPELINES_API_KEY" = "0p3n-w3bu!";
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

  systemd.services.open-webui = {
    serviceConfig = {
      # Disable the sandbox "lottery"
      DynamicUser = lib.mkForce false;
      User = "open-webui";
      Group = "open-webui";

      # Punch through the systemd sandbox
      ReadWritePaths = [ "/local/nixos/data/AI" ];
      ProtectHome = lib.mkForce false;
      StateDirectory = lib.mkForce ""; # Stop systemd from messsing with /var/lib
    };
  };

  systemd.tmpfiles.rules = [
    "d /local/nixos/data/AI 0755 root root -"

    # Ollama
    "d /local/nixos/data/AI/ollama 0700 ollama ollama -"
    "Z /local/nixos/data/AI/ollama 0700 ollama ollama -"

    # WebUI
    "d /local/nixos/data/AI/open-webui 0700 open-webui open-webui -"
    "Z /local/nixos/data/AI/open-webui 0700 open-webui open-webui -"

    # Pipelines
    "d /local/nixos/data/AI/pipelines 0700 root root -"
    "Z /local/nixos/data/AI/pipelines 0700 root root -"
  ];
  networking.firewall.interfaces."tailscale0".allowedTCPPorts = [ 8080 ];
}
