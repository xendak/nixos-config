{ lib, ... }:

{
  options.themes = {
    light = lib.mkOption {
      type = lib.types.attrs;
      default = { };
      description = "Attribute set for light base 16 theme";
    };

    dark = lib.mkOption {
      type = lib.types.attrs;
      default = { };
      description = "Attribute set for dark base16 theme";
    };

    default = lib.mkOption {
      type = lib.types.attrs;
      default = { };
      description = "Attribute set for default base16 theme";
    };
  };
}
