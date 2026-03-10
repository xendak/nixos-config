{
  lib,
  pkgs,
  config,
  inputs,
  ...
}:
let
  indent = {
    tab-width = 2;
    unit = "  ";
  };

  withUwu =
    lang:
    lang
    // {
      language-servers = (lang.language-servers or [ ]) ++ [ "uwu-colors" ];
    };

  prettier = lang: {
    command = lib.getExe pkgs.nodePackages.prettier;
    args = [
      "--parser"
      lang
    ];
  };

  rawLanguages = [
    {
      name = "nix";
      auto-format = true;
      language-servers = [ "nixd-lsp" ];
      formatter.command = lib.getExe pkgs.nixfmt;
    }
    {
      name = "odin";
      language-servers = [ "ols" ];
      inherit indent;
    }
    {
      name = "python";
      language-servers = [ "pyright" ];
      formatter = {
        command = lib.getExe pkgs.black;
        args = [
          "-"
          "--quiet"
        ];
      };
    }
    {
      name = "lua";
      language-servers = [ "lua-language-server" ];
      inherit indent;
    }
    {
      name = "fish";
      inherit indent;
      formatter.command = "fish_indent";
      language-servers = [ "fish-lsp" ];
    }
    {
      name = "bash";
      indent = {
        tab-width = 2;
        unit = " ";
      };
      formatter = {
        command = lib.getExe pkgs.shfmt;
        args = [
          "-i"
          "2"
        ];
      };
      language-servers = [ "bash-language-server" ];
    }
    {
      name = "javascript";
      auto-format = true;
      language-servers = [
        "dprint"
        "typescript-language-server"
      ];
    }
    {
      name = "typescript";
      auto-format = true;
      language-servers = [
        "dprint"
        "typescript-language-server"
      ];
    }
    {
      name = "markdown";
      text-width = 150;
      soft-wrap.enable = true;
      soft-wrap.wrap-at-text-width = true;
      formatter = {
        command = lib.getExe pkgs.deno;
        args = [
          "fmt"
          "-"
          "--ext"
          "md"
        ];
      };
      language-servers = [
        "dprint"
        "markdown-oxide"
      ];
    }
    {
      name = "toml";
      auto-format = false;
      inherit indent;
    }
    {
      name = "nu";
      inherit indent;
    }
  ];

  prettierLangsList =
    map
      (e: {
        name = e;
        formatter = prettier e;
      })
      [
        "css"
        "scss"
        "html"
        "json"
        "yaml"
      ];

in
{
  language-server = {
    uwu-colors.command = "${
      inputs.uwu-colors.packages.${pkgs.stdenv.hostPlatform.system}.default
    }/bin/uwu_colors";

    ols.command = lib.getExe pkgs.ols;
    pyright.command = lib.getExe pkgs.pyright;
    lua-language-server.command = lib.getExe pkgs.lua-language-server;

    rust-analyzer.config = {
      assist.importGranularity = "module";
      cargo.extraEnv."CARGO_TARGET_DIR" = "${config.xdg.cacheHome}/rust-analyzer-target-dir";
      check.command = "clippy";
      completion.fullFunctionSignatures.enable = true;
      inlayHints = {
        closingBraceHints.minLines = 10;
        closureReturnTypeHints.enable = "with_block";
        discriminantHints.enable = "fieldless";
      };
    };

    nixd-lsp.command = lib.getExe pkgs.nixd;
    bash-language-server.command = lib.getExe pkgs.bash-language-server;
    fish-lsp.command = lib.getExe pkgs.fish-lsp;
    typescript-language-server = {
      command = lib.getExe pkgs.nodePackages.typescript-language-server;
      args = [ "--stdio" ];
    };

    dprint = {
      command = lib.getExe pkgs.dprint;
      args = [ "lsp" ];
    };
  };

  language = map withUwu (rawLanguages ++ prettierLangsList);

  home.file.".dprint.json".source = builtins.toFile "dprint.json" (
    builtins.toJSON {
      lineWidth = 80;
      typescript = {
        quoteStyle = "preferSingle";
        binaryExpression.operatorPosition = "sameLine";
      };
      json.indentWidth = 2;
      plugins = [
        "https://plugins.dprint.dev/typescript-0.93.0.wasm"
        "https://plugins.dprint.dev/json-0.19.3.wasm"
        "https://plugins.dprint.dev/markdown-0.17.8.wasm"
      ];
    }
  );
}
