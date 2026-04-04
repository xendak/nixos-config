{ paletteSet, ... }:
let
  p = paletteSet.palette;

  theme = {
    name = "nix";
    type = if paletteSet.type == "light" then "light" else "dark";
    colors = {
      "editor.background" = p.background;
      "editor.foreground" = p.on_background;
      "editorCursor.foreground" = p.cursor_bg;
      "editor.selectionBackground" = p.selection_bg;
      "editorLineNumber.foreground" = p.gray;
      "editorLineNumber.activeForeground" = p.on_primary_container;
      "editorIndentGuide.background" = p.surface_container_high;
      "editor.lineHighlightBackground" = p.surface_container_low;
      "editor.lineHighlightBorder" = p.outline_variant;
    };
    tokenColors = [
      {
        scope = [
          "comment"
          "punctuation.definition.comment"
        ];
        settings = {
          foreground = p.comments;
          fontStyle = "italic";
        };
      }
      {
        scope = [
          "variable"
          "string constant.other.placeholder"
        ];
        settings = {
          foreground = p.on_background;
        };
      }
      {
        scope = [
          "constant"
          "entity.name.constant"
          "variable.other.constant"
          "variable.language"
        ];
        settings = {
          foreground = p.constants;
        };
      }
      {
        scope = [
          "entity.name.function"
          "support.function"
        ];
        settings = {
          foreground = p.functions;
        };
      }
      {
        scope = [
          "keyword"
          "storage.type"
          "storage.modifier"
        ];
        settings = {
          foreground = p.keywords;
        };
      }
      {
        scope = [ "string" ];
        settings = {
          foreground = p.strings;
        };
      }
      {
        scope = [ "constant.numeric" ];
        settings = {
          foreground = p.numeric;
        };
      }
      {
        scope = [
          "entity.name.type"
          "support.type"
          "support.class"
        ];
        settings = {
          foreground = p.types;
        };
      }
      {
        scope = [ "entity.name.tag" ];
        settings = {
          foreground = p.tags;
        };
      }
      {
        scope = [ "entity.other.attribute-name" ];
        settings = {
          foreground = p.modules;
        };
      }
      {
        scope = [
          "punctuation"
          "meta.brace"
        ];
        settings = {
          foreground = p.punctuation;
        };
      }
      {
        scope = [ "keyword.operator" ];
        settings = {
          foreground = p.constants;
        };
      }
    ];
  };
in
{
  "obsidian/shiki.json" = builtins.toJSON theme;
}
