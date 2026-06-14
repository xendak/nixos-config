{
  config,
  osConfig,
  ...
}:
{
  services.syncthing = {
    enable = true;

    guiAddress = "127.0.0.1:8384";
    guiCredentials = {
      username = config.home.username;
      passwordFile = osConfig.age.secrets.syncthing.path;
    };
  };

  home.persistence = {
    "/persist" = {
      directories = [
        ".local/state/syncthing"
      ];
    };
  };
}
