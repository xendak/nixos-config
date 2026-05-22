{
  inputs,
  lib,
  pkgs,
  ...
}:
let
  llama-cpp-rocm = pkgs.llama-cpp.override { rocmSupport = true; };
  llama-server-bin = "${llama-cpp-rocm}/bin/llama-server";
  modelPath = "/local/nixos/data/AI/models";

  ctx_size = "32768";
  llamaSwapYaml = pkgs.writeText "llama-swap.yaml" ''
    includeAliasesInList: true

    models:
      "Qwen":
        ttl: 300
        cmd: >
          env HSA_OVERRIDE_GFX_VERSION=10.3.0 ROC_ENABLE_PRE_VEGA=1 HIP_VISIBLE_DEVICES=0
          ${llama-server-bin}
          --port ''${PORT}
          --host 0.0.0.0
          --model ${modelPath}/Qwen3.6-35B-A3B-UD-Q4_K_XL.gguf
          --mmproj /local/nixos/data/AI/models/mmproj-BF16.gguf \
          -ngl 99
          --n-cpu-moe 18
          --fit on
          --fit-target 3072
          --ctx-size ${ctx_size}
          --fit-ctx ${ctx_size}
          --flash-attn on
          --no-mmap
          --parallel 1
          --jinja
          --batch-size 2048
          --ubatch-size 1024
          --cache-type-k q8_0
          --cache-type-v q8_0
        filters:
          stripParams: "temperature, top_p, top_k, min_p, presence_penalty, repeat_penalty"
          setParamsByID:
            "Qwen:instruct":
              chat_template_kwargs:
                enable_thinking: false
                preserve_thinking: false
              temperature: 0.7
              top_p: 0.8
              top_k: 20
              min_p: 0.0
              presence_penalty: 1.5
              repeat_penalty: 1.0
            "Qwen:thinking":
              chat_template_kwargs:
                enable_thinking: true
                preserve_thinking: true
              reasoning_budget: 4096
              temperature: 1.0
              top_p: 0.95
              top_k: 20
              min_p: 0.05
              presence_penalty: 1.5
              repeat_penalty: 1.0
            "Qwen:thinking-coding":
              chat_template_kwargs:
                enable_thinking: true
                preserve_thinking: true
              temperature: 0.6
              top_p: 0.95
              top_k: 20
              min_p: 0.0
              presence_penalty: 0.0
              repeat_penalty: 1.0
            "Qwen:instruct-reasoning":
              chat_template_kwargs:
                enable_thinking: false
                preserve_thinking: false
              temperature: 1.0
              top_p: 0.95
              top_k: 20
              min_p: 0.0
              presence_penalty: 1.5
              repeat_penalty: 1.0
  '';
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
    llama-cpp-rocm
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
    serviceConfig = {
      ExecStart = ''
        ${pkgs.llama-swap}/bin/llama-swap \
          -listen 0.0.0.0:11434 \
          -config ${llamaSwapYaml}
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
