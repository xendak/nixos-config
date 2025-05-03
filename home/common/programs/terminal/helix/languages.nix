{
  lib,
  pkgs,
  config,
  ...
}: let
  indent = {
    tab-width = 4;
    unit = "    ";
  };
in {
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
    clangd.args = [
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

    # nil.config = {
    #   nil_ls.settings.nil.nix.flake.autoEvalInputs = true;
    #   nil.formatting.command = [ "nixpkgs-fmt" ];
    # };

    nixd-lsp = {
      command = lib.getExe pkgs.nixd;
    };
  };

  language = [
    {
      name = "nix";
      language-servers = ["nixd-lsp"];
      auto-format = true;
      formatter.command = lib.getExe pkgs.nixfmt-rfc-style;
    }

    {
      name = "bash";
      inherit indent;
    }

    {
      name = "java";
      language-servers = ["scls" "jdtls"];
      roots = [ "pom.xml" ];
    }

    {
      name = "gherkin";
      scope = "source.gherkin";
      injection-regex = "^(gherkin|feature)?$";
      file-types = ["feature"];
      comment-token = "#";
      roots = [];
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
      name = "json";
      language-servers = ["efm-prettier" "vscode-json-language-server"];
    }

    {
      name = "markdown";
      text-width = 150;

      soft-wrap.enable = true;
      soft-wrap.wrap-at-text-width = true;
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
        args = ["--style=Google"];
      };
    }

    {
      name = "toml";
      auto-format = false;
      inherit indent;
    }
  ];
}
