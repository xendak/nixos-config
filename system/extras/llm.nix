{ pkgs, ... }:
{

  services.ollama = {
    enable = true;
    package = pkgs.ollama-rocm;

    home = "/local/nixos/data/AI/models";

    # loadModels = [
    #   "qwen3.5:35b "
    #   "qwen3.5:27b"
    # ];

    environmentVariables = {
      HSA_OVERRIDE_GFX_VERSION = "10.3.0";
      ROC_ENABLE_PRE_VEGA = "1";
      HCC_AMDGPU_TARGET = "gfx1030";

      # 15GB
      OLLAMA_MAX_VRAM = "15000000000";
    };

    rocmOverrideGfx = "10.3.0";
  };

  services.open-webui = {
    enable = true;

    stateDir = "/local/nixos/data/AI/open-webui";
    environment = {
      OLLAMA_API_BASE_URL = "http://127.0.0.1:11434";
      WEBUI_AUTH = "False";
    };

    host = "127.0.0.1";
    port = 8080;
  };
}
