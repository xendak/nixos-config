{
  pkgs,
  config,
  ...
}:
let
  m = builtins.elemAt config.monitors 0;
  hStr = toString m.height;

  # Resolution-to-Size Mapping
  resSettings = {
    "1440" = {
      fm = {
        h = "398";
        w = "600";
      };
    };
    "1080" = {
      fm = {
        h = "247";
        w = "350";
      };
    };
    "720" = {
      fm = {
        h = "140";
        w = "240";
      };
    };
  };

  # Fallback settings for unknown resolutions
  fallback = {
    fm = {
      h = "100";
      w = "200";
    };
    im = {
      h = "80";
      w = "20";
    };
  };

  cfg = resSettings.${hStr} or fallback;

  fm = cfg.fm;
in
{
  home.packages = with pkgs; [ rofi ];

  xdg.configFile."rofi/powermenu.sh".source =
    pkgs.writeShellScript "powermenu.sh"
      # bash
      ''
        theme="/home/${config.home.username}/.config/rofi/powermenu.rasi"
        lastlogin="$(last "$USER" | head -n1 | tr -s ' ' | cut -d' ' -f5,6,7)"
        uptime="$(uptime | sed -e 's/up //g')"
        host=$(hostname)

        shutdown=""; reboot=""; lock="󰌾"; suspend=""; logout=""
        yes=''; no=''

        rofi_cmd() {
          rofi -dmenu -p " $host@$USER" \
            -mesg " Last Login: $lastlogin |  Uptime: $uptime" \
            -theme "$theme"
        }

        confirm_cmd() {
          rofi -dmenu -p 'Confirmation' -mesg 'Are you Sure?' -theme "$theme" \
            -theme-str 'window {location: center; anchor: center; fullscreen: true;}' \
            -theme-str 'mainbox {children: [ "message", "listview" ]; margin: ${fm.h}px ${fm.w}px;}'
        }

        selected="$(echo -e "$lock\n$suspend\n$logout\n$reboot\n$shutdown" | rofi_cmd)"

        case ''${selected} in
          $shutdown) [[ $(echo -e "$yes\n$no" | confirm_cmd) == "$yes" ]] && systemctl poweroff ;;
          $reboot)   [[ $(echo -e "$yes\n$no" | confirm_cmd) == "$yes" ]] && systemctl reboot ;;
          $lock)     quickshell -p /home/${config.home.username}/Programming/xendak/nierlock/shell.qml ;;
          $suspend)  systemctl suspend ;;
          $logout)   pkill Hyprland ;;
        esac
      '';
}
