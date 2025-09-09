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
in
{
  language-server = {
    rust-analyzer.config = {
      # Use in local configs:
      #
      #: check.targets = [
      #:   "aarch64-apple-darwin",
      #:   "x86_64-pc-windows-msvc",
      #:   "x86_64-unknown-linux-musl",
      #: ]
      #:
      #: cargo.target = "x86_64-unknown-linux-musl"
      #: cargo.target = "x86_64-pc-windows-msvc"
      #: cargo.target = "aarch64-apple-darwin"

      assist.importGranularity = "module";
      cargo.extraEnv."CARGO_TARGET_DIR" = "${config.xdg.cacheHome}/rust-analyzer-target-dir";
      # A little slower than a simple check, but so useful
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
        # Reborrows and such
        expressionAdjustmentHints.enable = "never";
        expressionAdjustmentHints.hideOutsideUnsafe = false;
        expressionAdjustmentHints.mode = "prefer_prefix";
      };

      # I have beefy machines, let's use them
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
    vscode-json-language-server.config.provideFormatter = false;
    efm-prettier = {
      command = "efm-langserver";
      config = {
        documentFormatting = true;
        languages."=" = [
          {
            formatCommand = "prettier --stdin-filepath \${INPUT}";
            formatStdin = true;
          }
        ];
      };
    };

    jdtls = {
      command = lib.getExe pkgs.jdt-language-server;
      args = [
        "--jvm-arg=-javaagent:${pkgs.lombok}/share/java/lombok.jar"
        "-configuration"
        "${config.xdg.cacheHome}/.jdt/jdtls_install/config_linux"
        "-data"
        "${config.xdg.cacheHome}/.jdt/jdtls_data"
      ];
    };

    yaml-language-server.config.yaml.keyOrdering = false;
    # nil.config = {
    #   nil_ls.settings.nil.nix.flake.autoEvalInputs = true;
    #   nil.formatting.command = [ "nixpkgs-fmt" ];
    # };

    bash-language-server = {
      command = lib.getExe pkgs.bash-language-server;
      args = [ "start" ];
    };

    fish-lsp = {
      command = lib.getExe pkgs.fish-lsp;
      args = [ "start" ];
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
        "--all-scopes-completion"
        "--cross-file-rename"
        "--function-arg-placeholders=false"
        "--header-insertion=never"
        "--pch-storage=memory"
      ];
    };

    cmake-language-server = {
      command = lib.getExe pkgs.cmake-language-server;
    };

    tinymist = {
      command = lib.getExe pkgs.tinymist;
      config = {
        exportPdf = "onType";
        outputPath = "$root/target/$dir/$name";
        formatterMode = "typstyle";
        formatterPrintWidth = 80;
      };
    };

    typescript-language-server = {
      command = lib.getExe pkgs.nodePackages.typescript-language-server;
      args = [ "--stdio" ];
      config = {
        typescript-language-server.source = {
          addMissingImports.ts = true;
          fixAll.ts = true;
          organizeImports.ts = true;
          removeUnusedImports.ts = true;
          sortImports.ts = true;
        };
      };
    };

    uwu-colors = {
      command = "${inputs.uwu-colors.packages.${pkgs.system}.default}/bin/uwu_colors";
      # command = "uwu_colors"; # useful for testing
    };

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
        suggest = {
          completeFunctionCalls = false;
          imports = {
            hosts."https://deno.land" = true;
          };
        };
        inlayHints = {
          enumMemberValues.enabled = true;
          functionLikeReturnTypes.enabled = true;
          parameterNames.enabled = "all";
          parameterTypes.enabled = true;
          propertyDeclarationTypes.enabled = true;
          variableTypes.enabled = true;
        };
      };
    };

    dprint = {
      # command = "dprint";
      # TODO: fix this eventually
      command = lib.getExe pkgs.dprint;
      args = [ "lsp" ];
    };

    nixd-lsp = {
      command = lib.getExe pkgs.nixd;
    };
  };

  language =
    let
      deno = lang: {
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
      prettierLangs = map (e: {
        name = e;
        formatter = prettier e;
      });
      langs = [
        "css"
        "scss"
        "html"
      ];
    in
    [
      {
        name = "nix";
        auto-format = true;
        language-servers = [
          "nixd-lsp"
          "uwu-colors"
        ];
        formatter = {
          command = lib.getExe pkgs.nixfmt-rfc-style;
          args = [ ];
        };
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
      }

      {
        name = "java";
        language-servers = [
          "scls"
          "jdtls"
        ];
        roots = [ "pom.xml" ];
      }

      {
        name = "javascript";
        auto-format = true;
        language-servers = [
          "dprint"
          "typescript-language-server"
          "uwu-colors"
        ];
      }

      {
        name = "typescript";
        auto-format = true;
        language-servers = [
          "dprint"
          "typescript-language-server"
          "uwu-colors"
        ];
      }

      {
        name = "json";
        formatter = deno "json";
      }

      {
        name = "markdown";
        text-width = 150;
        soft-wrap.enable = true;
        soft-wrap.wrap-at-text-width = true;
        formatter = deno "md";

        language-servers = [
          "dprint"
          "markdown-oxide"
        ];
      }

      {
        name = "gherkin";
        scope = "source.gherkin";
        injection-regex = "^(gherkin|feature)?$";
        file-types = [ "feature" ];
        comment-token = "#";
        roots = [ ];
        inherit indent;

        auto-pairs = {
          "(" = ")";
          "[" = "]";
          "{" = "}";
          "$" = "$";
          "`" = "`";
          "\"" = ''"'';
        };
      }

      {
        name = "just";
        auto-format = false;
        inherit indent;
      }

      {
        name = "nu";
        inherit indent;
      }

      {
        name = "protobuf";
        inherit indent;
      }

      {
        name = "cpp";
        formatter = {
          command = "${pkgs.clang-tools}/bin/clang-format";
          args = [ "--style=Google" ];
        };
      }

      {
        name = "toml";
        auto-format = false;
        inherit indent;
      }
    ]
    ++ prettierLangs langs;

  home.file.".dprint.json".source = builtins.toFile "dprint.json" (
    builtins.toJSON {
      lineWidth = 80;

      # This applies to both JavaScript & TypeScript
      typescript = {
        quoteStyle = "preferSingle";
        binaryExpression.operatorPosition = "sameLine";
      };

      json.indentWidth = 2;

      excludes = [
        "**/*-lock.json"
      ];

      plugins = [
        "https://plugins.dprint.dev/typescript-0.93.0.wasm"
        "https://plugins.dprint.dev/json-0.19.3.wasm"
        "https://plugins.dprint.dev/markdown-0.17.8.wasm"
      ];
    }
  );
}
