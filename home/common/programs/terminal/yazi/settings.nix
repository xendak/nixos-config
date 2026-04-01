{ pkgs, config, ... }:
{
  programs.yazi.initLua =
    # lua
    ''
      require("zoxide"):setup({ update_db = true })      
      require("duckdb"):setup()
      require("augment-command"):setup({
        prompt = false,
        default_item_group_for_prompt = "hovered",
        smart_enter = true,
        smart_paste = false,
        smart_tab_create = false,
        smart_tab_switch = false,
        confirm_on_quit = true,
        open_file_after_creation = false,
        enter_directory_after_creation = false,
        use_default_create_behaviour = false,
        enter_archives = true,
        extract_retries = 3,
        recursively_extract_archives = true,
        preserve_file_permissions = false,
        encrypt_archives = false,
        encrypt_archive_headers = false,
        reveal_created_archive = true,
        remove_archived_files = false,
        must_have_hovered_item = true,
        skip_single_subdirectory_on_enter = true,
        skip_single_subdirectory_on_leave = true,
        smooth_scrolling = false,
        scroll_delay = 0.02,
        create_item_delay = 0.25,
        wraparound_file_navigation = true,
      })

      local function format_smart_time(timestamp)
      	if not timestamp or timestamp == 0 then return "" end

      	local now = os.date("*t")
      	local f = os.date("*t", timestamp)

      	if now.year ~= f.year then
      		return os.date("%y年%m月", timestamp)
      	end

      	if now.month == f.month and now.day == f.day then
      		return os.date("%H:%M", timestamp)
      	end

      	if now.month ~= f.month then
      		return os.date("%B%d日", timestamp)
      	end

      	return os.date("%x %H:%M", timestamp)
      end

      function Linemode:size_and_mtime()
      	local file = self._file
      	local size = file:size()
      	local size_str = size and ya.readable_size(size) or ""

      	if file.link_to then
      		local target = tostring(file.link_to)
      		if target:match("^/dev") or target:match("^/nix/") then
      			return size_str
      		end
      	end

      	if file.cha.is_dir then
      		return size and ya.readable_size(size) or ""
      	end

      	local time_str = format_smart_time(math.floor(file.cha.mtime or 0))

      	if size_str ~= "" and time_str ~= "" then
      		return string.format("%s %s", size_str, time_str)
      	else
      		return size_str .. time_str
      	end
      end

      function Linemode:mtime()
      	if self._file.cha.is_dir then return "" end
      	return format_smart_time(math.floor(self._file.cha.mtime or 0))
      end

      function Linemode:btime()
      	if self._file.cha.is_dir then return "" end
      	return format_smart_time(math.floor(self._file.cha.btime or 0))
      end

      function Linemode:my_default()
      	return self:size_and_mtime()
      end
      		'';

  programs.yazi.settings = {
    mgr = {
      linemode = "my_default";
    };
    # preview = {
    #   tab_size = 2;
    #   max_width = 600;
    #   max_height = 900;
    #   cache_dir = "";
    #   image_delay = 0;
    #   image_filter = "triangle";
    #   image_quality = 50;
    #   sixel_fraction = 15;
    #   ueberzug_scale = 1;
    #   ueberzug_offset = [
    # };

    plugin = {
      prepend_previewers = [
        {
          name = "*/";
          run = ''faster-piper -- eza -TL=3 --color=always --icons=always --group-directories-first --no-quotes "$1"'';
        }
        {
          name = "*.csv";
          run = "duckdb";
        }
        {
          name = "*.tsv";
          run = "duckdb";
        }
        {
          name = "*.json";
          run = "duckdb";
        }
        {
          name = "*.parquet";
          run = "duckdb";
        }
        {
          name = "*.xlsx";
          run = "duckdb";
        }
        {
          name = "*.db";
          run = "duckdb";
        }
        {
          name = "*.duckdb";
          run = "duckdb";
        }
        {
          mime = "{audio}/*";
          run = "mediainfo";
        }
        {
          mime = "*.tar*";
          run = ''faster-piper --format=url -- tar tf "$1"'';
        }
        {
          url = "*.md";
          run = ''faster-piper -- CLICOLOR_FORCE=1 glow -w=$w "$1"'';
        }
        {
          mime = "application/bittorrent";
          run = ''faster-piper -- transmission-show "$1"'';
        }
        {
          mime = "application/epub+zip";
          run = "epub-preview";
        }
        {
          url = "*.torrent";
          run = ''faster-piper -- transmission-show "$1"'';
        }
        {
          name = "*.mpk";
          run = "noop";
        }

      ];
      append_previewers = [
        {
          url = "*";
          run = ''faster-piper -- hexyl --border=none --terminal-width=$w "$1"'';
        }
      ];

      prepend_preloaders = [
        {
          name = "*.csv";
          run = "duckdb";
          multi = false;
        }
        {
          name = "*.tsv";
          run = "duckdb";
          multi = false;
        }
        {
          name = "*.json";
          run = "duckdb";
          multi = false;
        }
        {
          name = "*.parquet";
          run = "duckdb";
          multi = false;
        }
        {
          name = "*.xlsx";
          run = "duckdb";
          multi = false;
        }
        {
          mime = "{audio}/*";
          run = "mediainfo";
        }
        {
          name = "*.mpk";
          run = "noop";
        }
      ];
    };

    open = {
      rules = [
        {
          url = "*/";
          use = [
            "edit"
            "open"
            "emacs"
            "reveal"
          ];
        }
        {
          mime = "text/*";
          use = [
            "edit"
            "emacs"
            "open"
            "hexdump"
          ];
        }
        {
          mime = "image/*";
          use = [
            "open"
            "set-wallpaper"
            "reveal"
          ];
        }
        {
          mime = "{audio,video}/*";
          use = [
            "play"
            "reveal"
          ];
        }
        {
          mime = "application/{zip,rar,7z*,tar,gzip,xz,zstd,bzip*,lzma,compress,archive,cpio,arj,xar,ms-cab*}";
          use = [
            "extract"
            "reveal"
          ];
        }
        {
          mime = "application/{json,ndjson}";
          use = [
            "edit"
            "hexdump"
            "reveal"
          ];
        }
        {
          mime = "*/javascript";
          use = [
            "edit"
            "emacs"
            "reveal"
          ];
        }
        {
          mime = "inode/empty";
          use = [
            "edit"
            "reveal"
            "hexdump"
          ];
        }
        {
          mime = "vfs/{absent,stale}";
          use = "download";
        }
        {
          url = "*";
          use = [
            "open"
            "hexdump"
            "reveal"
          ];
        }
      ];

    };

    opener = {
      # TODO: think bout windows/mac eventually? if i ever go bk to them
      # ADD show media-info for images
      emacs = [
        {
          run = "${pkgs.emacs-pgtk}/bin/emacsclient -c -r -- %s";
          block = false;
          orphan = true;
          desc = "Open in Emacs";
          for = "linux";
        }
      ];
      open = [
        {
          run = "handlr open %s";
          desc = "Open";
        }
      ];
      reveal = [
        {
          run = "${config.home.sessionVariables.FILEBROWSER} \"$(dirname \"$1\")\"";
          desc = "Open in ${config.home.sessionVariables.FILEBROWSER}";
          orphan = true;
          for = "linux";
        }
      ];
      hexdump = [
        {
          run = "hexyl --border=none -- %s | bat -p --paging always --pager $PAGER";
          block = true;
          desc = "Open in hexview";
        }
      ];
      extract = [
        {
          run = "ya pub augmented-extract --list %s";
          desc = "Extract here";
        }
      ];
      set-wallpaper = [
        {
          run = "fish /home/${config.home.username}/Flake/home/common/programs/quickshell/niri/wallpaper.fish -f %s";
          desc = "Use as Wallpaper";
          for = "linux";
        }
      ];
    };
  };

}
