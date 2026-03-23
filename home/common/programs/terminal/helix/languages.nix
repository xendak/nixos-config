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

  denoFmt = lang: {
    command = lib.getExe pkgs.deno;
    args = [
      "fmt"
      "-"
      "--ext"
      lang
    ];
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
      formatter = {
        command = lib.getExe pkgs.nixfmt;
        args = [ ];
      };
    }
    {
      name = "go";
      auto-format = false;
      language-servers = [ "gopls" ];
      formatter = {
        command = lib.getExe' pkgs.gotools "gofmt";
        args = [ ];
      };
      inherit indent;
    }
    {
      name = "qml";
      language-servers = [ "qmlls" ];
      inherit indent;
    }
    {
      name = "rust";
      auto-format = true;
      language-servers = [ "rust-analyzer" ];
    }
    {
      name = "zig";
      auto-format = true;
      language-servers = [ "zls" ];
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
      name = "java";
      language-servers = [
        "scls"
        "jdtls"
      ];
      roots = [
        "pom.xml"
        "build.gradle"
      ];
      inherit indent;
    }
    {
      name = "dart";
      language-servers = [ "dart-lsp" ];
      auto-format = true;
    }
    {
      name = "c";
      language-servers = [ "clangd" ];
      inherit indent;
    }
    {
      name = "cpp";
      language-servers = [ "clangd" ];
      formatter = {
        command = "${pkgs.clang-tools}/bin/clang-format";
        args = [ "--style=Google" ];
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
      name = "tsx";
      auto-format = true;
      language-servers = [
        "dprint"
        "typescript-language-server"
      ];
    }
    {
      name = "jsx";
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
      formatter = denoFmt "md";
      language-servers = [
        "dprint"
        "markdown-oxide"
      ];
    }
    {
      name = "nu";
      inherit indent;
    }
    {
      name = "toml";
      auto-format = false;
      inherit indent;
    }
    {
      name = "protobuf";
      inherit indent;
    }
    {
      name = "just";
      auto-format = false;
      inherit indent;
    }
    {
      name = "gherkin";
      scope = "source.gherkin";
      file-types = [ "feature" ];
      inherit indent;
    }
  ];

  prettierLangs =
    map
      (e: {
        name = e;
        formatter = prettier e;
      })
      [
        "css"
        "scss"
        "html"
        "yaml"
      ];

  jsonLang = [
    {
      name = "json";
      formatter = denoFmt "json";
    }
  ];

in
{
  # :Language :Server
  language-server = {
    gopls = {
      command = lib.getExe pkgs.gopls;
      config.gopls = {
        hints = {
          assignVariableTypes = true;
          compositeLiteralFields = true;
          constantValues = true;
          functionTypeParameters = true;
          parameterNames = true;
          rangeVariableTypes = true;
        };
        analyses = {
          unusedparams = true;
          unreachable = true;
        };
        staticcheck = true;
      };
    };

    qmlls = {
      command = "${pkgs.kdePackages.qtdeclarative}/bin/qmlls";
      args = [ "-E" ];
    };

    rust-analyzer.config = {
      assist.importGranularity = "module";
      cargo.extraEnv."CARGO_TARGET_DIR" = "${config.xdg.cacheHome}/rust-analyzer-target-dir";
      check.command = "clippy";
      completion.fullFunctionSignatures.enable = true;
      hover.actions.references.enable = true;
      lens.references = {
        adt.enable = true;
        enumVariant.enable = true;
        method.enable = true;
        trait.enable = true;
      };
      inlayHints = {
        closingBraceHints.minLines = 10;
        closureReturnTypeHints.enable = "with_block";
        discriminantHints.enable = "fieldless";
        lifetimeElisionHints.enable = "skip_trivial";
        typeHints.hideClosureInitialization = false;
        expressionAdjustmentHints.enable = "never";
        expressionAdjustmentHints.hideOutsideUnsafe = false;
        expressionAdjustmentHints.mode = "prefer_prefix";
      };
      lruCapacity = 256;
      workspace.symbol.search = {
        limit = 128;
        kind = "all_symbols";
        scope = "workspace";
      };
      diagnostics.disabled = [
        "inactive-code"
        "inactive_code"
        "unresolved-proc-macro"
        "unresolved_proc_macro"
      ];
    };

    clangd = {
      command = "${pkgs.clang-tools}/bin/clangd";
      clangd.fallbackFlags = [ "-std=c++2b" ];
      args = [
        "--inlay-hints"
        "--background-index"
        "--offset-encoding=utf-16"
        "--compile-commands-dir=build"
        "--completion-style=detailed"
        "--all-scopes-completion=true"
        "--recovery-ast"
        "--suggest-missing-includes"
        "--clang-tidy"
        "--cross-file-rename"
        "--function-arg-placeholders=false"
        "--header-insertion=never"
        "--pch-storage=memory"
      ];
    };

    jdtls = {
      command = lib.getExe pkgs.jdt-language-server;
      args = [
        "--jvm-arg=-javaagent:${(pkgs.lombok.override { jdk = pkgs.jdk25; })}/share/java/lombok.jar"
        "-configuration"
        "${config.xdg.cacheHome}/.jdt/jdtls_install/config_linux"
        "-data"
        "${config.xdg.cacheHome}/.jdt/jdtls_data"
      ];
    };

    typescript-language-server = {
      command = lib.getExe pkgs.nodePackages.typescript-language-server;
      args = [ "--stdio" ];
      config.typescript-language-server.source = {
        addMissingImports.ts = true;
        fixAll.ts = true;
        organizeImports.ts = true;
        removeUnusedImports.ts = true;
        sortImports.ts = true;
      };
    };

    nixd-lsp.command = lib.getExe pkgs.nixd;
    zls.command = lib.getExe pkgs.zls;
    ols.command = lib.getExe pkgs.ols;
    pyright.command = lib.getExe pkgs.pyright;
    dart-lsp = {
      command = "dart";
      args = [ "language-server" ];
    };
    bash-language-server = {
      command = lib.getExe pkgs.bash-language-server;
      args = [ "start" ];
    };
    fish-lsp = {
      command = lib.getExe pkgs.fish-lsp;
      args = [ "start" ];
    };
    lua-language-server.command = lib.getExe pkgs.lua-language-server;
    cmake-language-server.command = lib.getExe pkgs.cmake-language-server;
    tinymist = {
      command = lib.getExe pkgs.tinymist;
      config = {
        exportPdf = "onType";
        outputPath = "$root/target/$dir/$name";
        formatterMode = "typstyle";
        formatterPrintWidth = 80;
      };
    };
    uwu-colors.command = "${
      inputs.uwu-colors.packages.${pkgs.stdenv.hostPlatform.system}.default
    }/bin/uwu_colors";
    vscode-css-language-server = {
      command = "${pkgs.nodePackages.vscode-langservers-extracted}/bin/vscode-css-language-server";
      args = [ "--stdio" ];
      config = {
        provideFormatter = true;
        css.validate.enable = true;
        scss.validate.enable = true;
      };
    };
    deno-lsp = {
      command = lib.getExe pkgs.deno;
      args = [ "lsp" ];
      environment.NO_COLOR = "1";
      config.deno = {
        enable = true;
        lint = true;
        unstable = true;
        inlayHints = {
          enumMemberValues.enabled = true;
          functionLikeReturnTypes.enabled = true;
          parameterNames.enabled = "all";
          parameterTypes.enabled = true;
          variableTypes.enabled = true;
        };
      };
    };
    dprint = {
      command = lib.getExe pkgs.dprint;
      args = [ "lsp" ];
    };
  };

  # rely on helix lsp for documentColor' aware lsp's
  language = map withUwu rawLanguages ++ prettierLangs ++ jsonLang;

  home.file.".dprint.json".source = builtins.toFile "dprint.json" (
    builtins.toJSON {
      lineWidth = 80;
      typescript = {
        quoteStyle = "preferSingle";
        binaryExpression.operatorPosition = "sameLine";
      };
      json.indentWidth = 2;
      excludes = [ "**/*-lock.json" ];
      plugins = [
        "https://plugins.dprint.dev/typescript-0.93.0.wasm"
        "https://plugins.dprint.dev/json-0.19.3.wasm"
        "https://plugins.dprint.dev/markdown-0.17.8.wasm"
      ];
    }
  );
}
