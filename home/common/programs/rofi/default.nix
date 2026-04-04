{
  pkgs,
  config,
  ...
}:
let
  m = builtins.elemAt config.monitors 0;
  fm_height =
    if m.height == 1440 then
      "398"
    else if m.height == 1080 then
      "247"
    else
      "200";
  fm_width =
    if m.height == 1440 then
      "600"
    else if m.height == 1080 then
      "350"
    else
      "200";
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
            -theme-str 'mainbox {children: [ "message", "listview" ]; margin: ${fm_height}px ${fm_width}px;}'
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
