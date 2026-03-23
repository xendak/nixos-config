{
  pkgs,
  lib,
  ...
}:
{
  imports = [
    ./lsp.nix
    ./syntaxes.nix
    ./ui.nix
    ./copilot.nix
    ./plugins.nix
  ];
  home.sessionVariables.EDITOR = lib.mkDefault "nvim";

  programs.neovim = {
    enable = true;

    # extraRuntime = {
    #   "colors/nix-${config.colorscheme.slug}.vim" = {
    #     text = import ./theme.nix config.colorscheme;
    #   };
    # };

    extraConfig =
      # vimsc
      ''
        "Use system clipboard
        set clipboard=unnamedplus
        "Use truecolor
        set termguicolors

        "Set fold level to highest in file
        "so everything starts out unfolded at just the right level
        augroup initial_fold
          autocmd!
          autocmd BufWinEnter * let &foldlevel = max(map(range(1, line('$')), 'foldlevel(v:val)'))
        augroup END

        "Tabs
        set tabstop=4 "4 char-wide tab
        set expandtab "Use spaces
        set softtabstop=0 "Use same length as 'tabstop'
        set shiftwidth=0 "Use same length as 'tabstop'
        "2 char-wide overrides
        augroup two_space_tab
          autocmd!
          autocmd FileType json,html,htmldjango,hamlet,nix,scss,typescript,php,haskell,terraform setlocal tabstop=2
        augroup END

        "Set tera to use htmldjango syntax
        augroup tera_htmldjango
          autocmd!
          autocmd BufRead,BufNewFile *.tera setfiletype htmldjango
        augroup END

        "Disable latex on .txt
        augroup latex_settings
          autocmd!
          autocmd LspAttach,BufRead,BufNewFile *.txt silent exec "LspStop"
        augroup END

        "Rust
        augroup rust_settings
          autocmd!
          autocmd BufRead,BufNewFile *.rs nmap <C-c> :!cargo run<CR>
        augroup END

        "Options when composing mutt mail
        augroup mail_settings
          autocmd FileType mail set noautoindent wrapmargin=0 textwidth=0 linebreak wrap formatoptions +=w
        augroup END

        "Fix nvim size according to terminal
        "(https://github.com/neovim/neovim/issues/11330)
        augroup fix_size
          autocmd VimEnter * silent exec "!kill -s SIGWINCH" getpid()
        augroup END

        "Line numbers
        set number relativenumber

        nmap <C-s> :w<CR>
        "Scroll up and down
        nmap <C-j> <C-e>
        nmap <C-k> <C-y>

        "Buffers
        nmap <C-l> :bnext<CR>
        nmap <C-h> :bprev<CR>
        nmap <C-q> :bdel<CR>

        "Loclist
        nmap <space>l :lwindow<cr>
        nmap [l :lprev<cr>
        nmap ]l :lnext<cr>

        nmap <space>L :lhistory<cr>
        nmap [L :lolder<cr>
        nmap ]L :lnewer<cr>

        "Quickfix
        nmap <space>q :cwindow<cr>
        nmap [q :cprev<cr>
        nmap ]q :cnext<cr>

        nmap <space>Q :chistory<cr>
        nmap [Q :colder<cr>
        nmap ]Q :cnewer<cr>

        "Make
        nmap <space>m :make<cr>

        "Grep (replace with ripgrep)
        nmap <space>g :grep<space>
        if executable('rg')
            set grepprg=rg\ --vimgrep
            set grepformat=%f:%l:%c:%m
        endif

        "Close other splits
        nmap <space>o :only<cr>

        "Telescope and nnn
        let g:nnn#command = 'nnn -deiH'
        nmap <space>F :NnnPicker %:p:h<CR>
        "nmap <space><space> :Telescope fd<CR>
        "nmap <space>// :Telescope live_grep<CR>
        nmap <space>T :Telescope<CR>

        "Sudo save
        cmap w!! w !sudo tee > /dev/null %
      '';
    initLua =
      # lua
      ''
        -- color
        vim.cmd('source ' .. vim.fn.expand('~/.config/nvim/colors.vim'))

        -- telescope
        vim.keymap.set("n", "<space><space>", require("telescope.builtin").find_files, {desc = "Telescope find files"})
        vim.keymap.set("n", "<space>b", require("telescope.builtin").buffers, {desc = "Telescope buffers"})
        vim.keymap.set("n", "<space>/", require("telescope.builtin").live_grep, {desc = "Telescope live grep"})

        vim.keymap.set("n", "gD", vim.lsp.buf.declaration, { desc = "Go to declaration" })
        vim.keymap.set("n", "gd", vim.lsp.buf.definition, { desc = "Go to definition" })
        vim.keymap.set("n", "gi", vim.lsp.buf.implementation, { desc = "Go to implementation" })
        vim.keymap.set("n", "<space>f", vim.lsp.buf.format, { desc = "Format code" })
        vim.keymap.set("n", "K", vim.lsp.buf.hover, { desc = "Hover Documentation" })
        vim.keymap.set("n", "<space>c", vim.lsp.buf.code_action, { desc = "Code action" })

        -- Diagnostic
        vim.keymap.set("n", "<space>e", vim.diagnostic.open_float, { desc = "Floating diagnostic" })
        vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, { desc = "Previous diagnostic" })
        vim.keymap.set("n", "]d", vim.diagnostic.goto_next, { desc = "Next diagnostic" })
        vim.keymap.set("n", "gl", vim.diagnostic.setloclist, { desc = "Diagnostics on loclist" })
        vim.keymap.set("n", "gq", vim.diagnostic.setqflist, { desc = "Diagnostics on quickfix" })

        -- Terminal mode
        vim.keymap.set("t", "<ESC>", "<C-\\><C-n>", {desc = "ESC to change to normal mode in terminal"})

        function add_sign(name, text)
          vim.fn.sign_define(name, { text = text, texthl = name, numhl = name})
        end

        -- Compilation Mode
        vim.g.compile_mode = {}
        vim.api.nvim_create_autocmd("FileType", {
            pattern = "compilation",
            callback = function(args)
                local buf = args.buf

                -- TODO: expand this based on gobuild
                local function parse_error_line(line)
                    local file, lnum, col = line:match("^([^%(\n]+)%((%d+):(%d+)%)")
                    if file then return file, lnum, col end

                    file, lnum, col = line:match("^([^:\n]+):(%d+):(%d+)")
                    if file then return file, lnum, col end

                    file, lnum = line:match("^([^:\n]+):(%d+)")
                    if file then return file, lnum, "1" end

                    file, lnum = line:match('^%s*File "(.-)", line (%d+)')
                    if file then return file, lnum, "1" end

                    return nil, nil, nil
                end

                vim.keymap.set('n', '<CR>', function()
                    local line = vim.api.nvim_get_current_line()
                    local file, lnum, col = parse_error_line(line)

                    if file and lnum then
                        vim.cmd('wincmd p')
                        vim.cmd('edit ' .. vim.fn.fnameescape(file))
                        vim.api.nvim_win_set_cursor(0, {tonumber(lnum), math.max(0, tonumber(col) - 1)})
                        vim.cmd('normal! zz')
                    else
                        vim.notify("No valid file path found on this line.", vim.log.levels.WARN)
                    end
                end, { buffer = buf })

                local function jump_to_error(direction)
                    local current_line = vim.api.nvim_win_get_cursor(0)[1]
                    local total_lines = vim.api.nvim_buf_line_count(buf)
                    local step = (direction == "next") and 1 or -1
                    local curr = current_line + step

                    while curr >= 1 and curr <= total_lines do
                        local line = vim.api.nvim_buf_get_lines(buf, curr - 1, curr, false)[1]
                        local file = parse_error_line(line)
              
                        if file then
                            vim.api.nvim_win_set_cursor(0, {curr, 0})
                            return
                        end
                        curr = curr + step
                    end
                    vim.notify("No more errors found in this direction.", vim.log.levels.INFO)
                end

                vim.keymap.set('n', 'n', function() jump_to_error("next") end, { buffer = buf, silent = true })
                vim.keymap.set('n', 'N', function() jump_to_error("prev") end, { buffer = buf, silent = true })
            end
        })

        add_sign("DiagnosticSignError", " ")
        add_sign("DiagnosticSignWarn", " ")
        add_sign("DiagnosticSignHint", " ")
        add_sign("DiagnosticSignInfo", " ")
      '';

    plugins = with pkgs.vimPlugins; [
      vim-table-mode
      editorconfig-nvim
      vim-surround
      {
        plugin = nvim-autopairs;
        type = "lua";
        config =
          # lua
          ''
            require('nvim-autopairs').setup{}
          '';
      }
    ];
  };

  xdg.configFile."nvim/init.lua".onChange = ''
    XDG_RUNTIME_DIR=''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}
    for server in $XDG_RUNTIME_DIR/nvim.*; do
      nvim --server $server --remote-send ':source $MYVIMRC<CR>' &
    done
  '';

  xdg.desktopEntries = {
    nvim = {
      name = "Neovim";
      genericName = "Text Editor";
      comment = "Edit text files";
      exec = "nvim %F";
      icon = "nvim";
      mimeType = [
        "text/english"
        "text/plain"
        "text/x-makefile"
        "text/x-c++hdr"
        "text/x-c++src"
        "text/x-chdr"
        "text/x-csrc"
        "text/x-java"
        "text/x-moc"
        "text/x-pascal"
        "text/x-tcl"
        "text/x-tex"
        "application/x-shellscript"
        "text/x-c"
        "text/x-c++"
      ];
      terminal = true;
      type = "Application";
      categories = [
        "Utility"
        "TextEditor"
      ];
    };
  };
}
