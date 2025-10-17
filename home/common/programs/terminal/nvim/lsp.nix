{
  pkgs,
  lib,
  config,
  ...
}:
{
  programs.neovim.plugins = with pkgs.vimPlugins; [
    # LSP and completions for injected langs
    otter-nvim

    # LSP
    {
      plugin = nvim-lspconfig;
      type = "lua";
      config =
        # lua
        ''
          vim.lsp.enable("dockerls", {})
          vim.lsp.enable("bashls", {})
          vim.lsp.enable("clangd", {
            cmd =  {
              "clangd",
              "--inlay-hints",
              "--offset-encoding=utf-16",
              "--compile-commands-dir=build",
              "--background-index",
              "--completion-style=detailed",
              "--all-scopes-completion=true",
              "--recovery-ast",
              "--suggest-missing-includes",
              "--clang-tidy",
              "--all-scopes-completion",
              "--cross-file-rename",
              "--function-arg-placeholders=false",
              "--header-insertion=never",
              "--pch-storage=memory",
            }
          })
          vim.lsp.enable("nixd", {
            settings = { nixd = {
              formatting = { command = { "alejandra" }}
            }}
          })
          vim.lsp.enable("pylsp", {})
          vim.lsp.enable("dartls", {})
          vim.lsp.enable("hls", {})
          vim.lsp.enable("kotlin_language_server", {})
          vim.lsp.enable("solargraph", {})
          vim.lsp.enable("phpactor", {})
          vim.lsp.enable("terraformls", {})
          vim.lsp.enable("gopls", {})
          vim.lsp.enable("lua_ls", {})
          vim.lsp.enable("jdtls", {
            cmd = {
              "${lib.getExe pkgs.jdt-language-server}",
              "--jvm-arg=-javaagent:${pkgs.lombok}/share/java/lombok.jar",
              "-configuration",
              "${config.xdg.cacheHome}/.jdt/jdtls_install/config_linux",
              "-data",
              "${config.xdg.cacheHome}/.jdt/jdtls_data"
            }
          })
          vim.lsp.enable("zls", {})
          vim.lsp.enable("ts_ls", {})
          vim.lsp.enable("tinymist", {})
          vim.lsp.enable("elixirls", { cmd = {"elixir-ls"}})

          vim.lsp.enable("texlab", {
            chktex = { onEdit = true, onOpenAndSave = true }
          })
        '';
    }
    {
      plugin = ltex_extra-nvim;
      type = "lua";
      config =
        # lua
        ''
          local ltex_extra = require('ltex_extra')
          vim.lsp.enable("ltex", {
            on_attach = function(client, bufnr)
              ltex_extra.setup{
                path = vim.fn.expand("~") .. "/.local/state/ltex"
              }
            end
          })
        '';
    }
    {
      plugin = rust-tools-nvim;
      type = "lua";
      config =
        # lua
        ''
          local rust_tools = require('rust-tools')
          vim.lsp.config('rust-analyzer', {
            cmd = { "rust-analyzer" },
            tools = { autoSetHints = true }
          })
          vim.lsp.enable("rust-analyzer", {
            cmd = { "rust-analyzer" },
            tools = { autoSetHints = true }
          })
          vim.api.nvim_set_hl(0, '@lsp.type.comment.rust', {})
        '';
    }

    # Snippets
    luasnip

    # Completions
    cmp-nvim-lsp
    cmp_luasnip
    cmp-rg
    cmp-buffer
    cmp-path
    {
      plugin = cmp-git;
      type = "lua";
      config =
        # lua
        ''
          require("cmp_git").setup({})
        '';
    }

    lspkind-nvim
    {
      plugin = nvim-cmp;
      type = "lua";
      config =
        # lua
        ''
          local cmp = require('cmp')

          cmp.setup({
            formatting = {
              format = require('lspkind').cmp_format({
                before = function (entry, vim_item)
                  return vim_item
                end,
              }),
            },
            snippet = {
              expand = function(args)
                require("luasnip").lsp_expand(args.body)
              end,
            },
            mapping = cmp.mapping.preset.insert({
            }),
            sources = {
              { name='otter' },
              { name='nvim_lsp' },
              { name='luasnip' },
              { name='git' },
              { name='buffer', option = { get_bufnrs = vim.api.nvim_list_bufs }},
              { name='path' },
              { name='rg' },
            },
          })
        '';
    }
  ];
}
