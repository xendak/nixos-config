{
  config,
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [ rustdesk-flutter ];

  home.persistence = {
    "/persist" = {
      directories = [
        ".config/rustdesk"
      ];
    };
  };
}
