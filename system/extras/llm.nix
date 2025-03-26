{...}: {
  services.ollama = {
    enable = true;
    acceleration = "rocm";
    loadModels = ["deepseek-r1:32b"];
    environmentVariables = {
      ROC_ENABLE_PRE_VEGA = "1";
      HSA_OVERRIDE_GFX_VERSION = "11.0.1";
      HCC_AMDGPU_TARGET = "gfx1101";
    };
    rocmOverrideGfx = "11.0.1";
  };
  services.open-webui = {
    enable = true;
    environment.OLLAMA_API_BASE_URL = "http://localhost:11434";
  };
}
