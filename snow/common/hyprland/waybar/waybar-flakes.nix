{ config, lib, pkgs, user, ... }:
{
  # nixpkgs.overlays = [
  #   (final: prev: {
  #     waybar =
  #       prev.waybar.overrideAttrs (oldAttrs: {
  #         mesonFlags = oldAttrs.mesonFlags ++ [ "-Dexperimental=true" ];
  #         buildInputs = oldAttrs.buildInputs ++ [ prev.fmt_9 ];
  #       });
  #   })
  # ];
  nixpkgs.overlays = [
    (final: prev: let
      waybarSrc = fetchFromGitHub {
        owner = "NixOS";
        repo = "nixpkgs";
        rev = "4afc316e4272c34429d031e290c6b5a7ed975875";
        sha256 = "1wgnlzc85y46c5rhwlvwrq99igw62q6nzk42rrnmfcjc7fmyfk90";
      };
    in prev // {
      waybar = prev.waybar.override {
        src = waybarSrc;
        mesonFlags = prev.waybar.mesonFlags ++ [ "-Dexperimental=true" ];
      };
    })
  ];

  xdg.configFile."waybar/powermenu.sh".source = pkgs.writeShellScript "powermenu.sh" ''
    ## Author : Aditya Shakya (adi1090x)
    ## Github : @adi1090x

    # Current Theme
    theme='~/.config/rofi/powermenu.rasi'

    # CMDs
    lastlogin="`last $USER | head -n1 | tr -s ' ' | cut -d' ' -f5,6,7`"
    uptime="`uptime | sed -e 's/up //g'`"
    host=`hostname`

    # Options
    shutdown=""
    reboot=""
    lock=""
    suspend=""
    logout=""
    yes=''
    no=''

    # Rofi CMD
    rofi_cmd() {
      rofi -dmenu \
        -p " $host@$USER" \
        -mesg " Last Login: $lastlogin |  Uptime: $uptime" \
        -theme "$theme"
    }

    # Confirmation CMD
    confirm_cmd() {
      rofi -theme-str 'window {location: center; anchor: center; fullscreen: true; width: 350px;}' \
        -theme-str 'mainbox {children: [ "message", "listview" ]; margin: 575px 950px;}' \
        -theme-str 'listview {columns: 2; lines: 1;}' \
        -theme-str 'element-text {horizontal-align: 0.5;}' \
        -theme-str 'textbox {horizontal-align: 0.5;}' \
        -theme-str 'element {padding: 30px;}' \
        -dmenu \
        -p 'Confirmation' \
        -mesg 'Are you Sure?' \
        -theme "$theme"
    }

    # Ask for confirmation
    confirm_exit() {
      echo -e "$yes\n$no" | confirm_cmd
    }

    # Pass variables to rofi dmenu
    run_rofi() {
      echo -e "$lock\n$suspend\n$logout\n$reboot\n$shutdown" | rofi_cmd
    }

    # Execute Command
    run_cmd() {
      selected="$(confirm_exit)"
      if [[ "$selected" == "$yes" ]]; then
        if [[ $1 == '--shutdown' ]]; then
          systemctl poweroff
        elif [[ $1 == '--reboot' ]]; then
          systemctl reboot
        elif [[ $1 == '--suspend' ]]; then
          mpc -q pause
          wpctl set-mute @DEFAULT_SINK@ toggle
          systemctl suspend
        elif [[ $1 == '--logout' ]]; then
                pkill Hyprland
        fi
      else
        exit 0
      fi
    }

    # Actions
    chosen="$(run_rofi)"
    case ''${chosen} in
        $shutdown)
        run_cmd --shutdown
            ;;
        $reboot)
        run_cmd --reboot
            ;;
        $lock)
        if [[ -x '/usr/bin/betterlockscreen' ]]; then
          betterlockscreen -l
        elif [[ -x '/usr/bin/i3lock' ]]; then
          i3lock
        fi
            ;;
        $suspend)
        run_cmd --suspend
            ;;
        $logout)
        run_cmd --logout
            ;;
    esac
  '';

  xdg.configFile."waybar/sound_sink_toggle.sh".source = pkgs.writeShellScript "sound_sink_toggle.sh" ''
    devices=()

    # Find existing audio devices and their IDs
    speaker=$(pw-cli ls Node | grep "output.pci" | grep -v "iec" | awk -F ' = ' '{print $2}')
    if [ ! -z "$speaker" ]; then
        speaker_id=$(pw-dump Node Device | jq '.[].info.props|select(."node.name" == '"$speaker"') | ."object.id"')
        devices+=("$speaker_id")
        echo "Speaker exists: $speaker_id"
    fi

    bluetooth=$(pw-cli ls Node | grep "output.bluetooth" | cut -d'=' -f2)
    if [ ! -z "$bluetooth" ]; then
        bluetooth_id=$(pw-dump Node Device | jq '.[].info.props|select(."node.name" == '"$bluetooth"') | ."object.id"')
        devices+=("$bluetooth_id")
        echo "Bluetooth exists: $bluetooth_id"
    fi

    headset=$(pw-cli ls Node | grep "output.usb" | cut -d'=' -f2)
    if [ ! -z "$headset" ]; then
        headset_id=$(pw-dump Node Device | jq '.[].info.props|select(."node.name" == '"$headset"') | ."object.id"')
        devices+=("$headset_id")
        echo "Headset exists: $headset_id"
    fi

    # Get the current default sink ID
    default_sink_name=$(pw-metadata 0 'default.audio.sink' | grep 'value' | sed "s/.* value:'//;s/' type:.*$//;" | jq .name)
    default_sink_id=$(pw-dump Node Device | jq '.[].info.props|select(."node.name" == '"$default_sink_name"') | ."object.id"')

    # Set the next device in the list as the default sink
    current_device_index=0
    for device_id in "''${devices[@]}"; do
        if [[ "$default_sink_id" == "$device_id" ]]; then
            current_device_index=$((current_device_index + 1))
            next_device_index=$((current_device_index % ''${#devices[@]}))
            next_device_id=''${devices[$next_device_index]}
            wpctl set-default "$next_device_id"
            echo "Default sink set to $next_device_id"
            exit 0
        fi
        current_device_index=$((current_device_index + 1))
    done

    # If the current default sink is not in the list, set the first device in the list as the default sink
    if [ "''${#devices[@]}" -gt 0 ]; then
        wpctl set-default "''${devices[0]}"
        echo "Default sink set to ''${devices[0]}"
    fi
  '';

  xdg.configFile."waybar/get-media-player.sh".source = pkgs.writeShellScript "get-media-player.sh" ''
    #!/bin/sh
    player=$(playerctl --player=i,com,spotify,firefox,%any metadata --format '{{playerName}}')
    player_name=$player
    exec $player_name
  '';

  programs.waybar = {
    enable = true;
    systemd = {
      enable = false;
      target = "graphical-session.target";
    };

    style = ''
      *
      {
        font-family: "Source Han Code JP", "Font Awesome 6 Duotone";
        font-size: 14px;
      }

      @define-color base   #${config.colorscheme.colors.base00};
      @define-color mantle #${config.colorscheme.colors.base02};
      @define-color crust  #${config.colorscheme.colors.base01};

      @define-color text     #${config.colorscheme.colors.base05};
      @define-color subtext0 #a6adc8;
      @define-color subtext1 #bac2de;

      @define-color surface0 #${config.colorscheme.colors.base00};
      @define-color surface1 #${config.colorscheme.colors.base02};
      @define-color surface2 #${config.colorscheme.colors.base01};

      @define-color overlay0 #6c7086;
      @define-color overlay1 #7f849c;
      @define-color overlay2 #9399b2;

      @define-color blue      #${config.colorscheme.colors.base0D};
      @define-color lavender  #${config.colorscheme.colors.base0C};
      @define-color sapphire  #74c7ec;
      @define-color sky       #89dceb;
      @define-color teal      #${config.colorscheme.colors.base05};
      @define-color green     #${config.colorscheme.colors.base0B};
      @define-color yellow    #${config.colorscheme.colors.base0A};
      @define-color peach     #fab387;
      @define-color maroon    #eba0ac;
      @define-color red       #${config.colorscheme.colors.base08};
      @define-color mauve     #cba6f7;
      @define-color pink      #f5c2e7;
      @define-color flamingo  #f2cdcd;
      @define-color rosewater #f5e0dc;

      @keyframes blink_critical {
        to {
          color: #${config.colorscheme.colors.base09};
          background-color: @red;
        }
      }

      .warning, .critical {
        animation-name: blink_critical;
        animation-duration: 1s;
        animation-timing-function: linear;
        animation-iteration-count: infinite;
        animation-direction: alternate;
      }

      .critical {
        animation-duration: 1s;
      }

      * {
        font-family: Source Han Code JP;
        font-weight: bold;
      }

      window#waybar {
        background-color: transparent;
      }

      window#waybar.hidden {
        opacity: 0.5;
      }

      window > box {
        border-color: @crust; 
        border-radius: 4px;
        border-style: solid; 
        background-color: transparent;/* Keep transparent to blur */
      }

      #memory, #cpu, #temperature, #clock, #calendar, #custom-calendar, #network , #pulseaudio, #backlight, #battery, #workspaces, #window, #tray, #mpris {
        padding: 6px 10px;
        margin: 0 3px;
        color: @text;
        border-radius: 16px;
        border-width: 0 2px 2px 0;
        border-style: solid;
        border-color: @crust;
        background-color: @surface0;
      }

      #custom-weather, #custom-bluetooth-battery, #custom-mpris-media, #custom-power-menu, #custom-recording {
        padding: 6px 10px;
        margin: 0 3px;
        color: @text;
        border-radius: 16px;
        border-width: 0 2px 2px 0;
        border-style: solid;
        border-color: @crust;
        background-color: @surface0;
      }
      #custom-start-menu {
        padding: 0px 0px;
        margin: 0 0px;
        color: @text;
        border-radius: 16px;
        border-width: 0 2px 2px 0;
        border-style: solid;
        border-color: @crust;
        background-color: @surface0;
      }
      #custom-song-previous, #custom-song-toggle, #custom-song-next {
        padding: 6px 14px;
        margin: 0px 3px;
        color: @text;
        border-radius: 16px;
        border-width: 0 2px 2px 0;
        border-style: solid;
        border-color: @crust;
        background-color: @surface0;
      }

      #workspaces button {
        transition: none;
        color: @text;
        background: transparent;
        font-size: 16px;
      }

      #workspaces button:hover {
        transition: none;
        box-shadow: inherit;
        text-shadow: inherit;
        color: @red;
      }

      #workspaces button.active {
        color: @surface1;
        background-color: @green;
        border-radius: 10px;
      }

      .modules-left, .modules-center, .modules-right {
        padding: 5px 0;
        margin: 0 10px;
      }

      tooltip {
        background: @surface0;
        padding: 15px;
        border-radius: 10px;
        box-shadow: inherit;
        border-width: 0 2px 2px 0;
        border-style: solid;
        border-color: @crust;
      }

      tooltip label {
        color: @text;
        font-size: small;
      }

      #battery.charging {
        animation-name: none;
      }
  '';

    settings = 
    [
      {
        "layer" = "top";
        "output" = "DP-1";

        "modules-left" = [
          "custom/start-menu"
          "wlr/workspaces"
          # "custom/mpris-media"
          "custom/song-previous"
          "custom/song-toggle"
          "custom/song-next"
        ];

        "modules-center" = [
          "hyprland/window"
        ];

        "modules-right" = [
          "tray"
          # "custom/bluetooth-battery"
          # "custom/recording"
          # "cpu"
          # "temperature"
          # "network"
          "pulseaudio"
          # "backlight"
          # "battery"
          "clock"
          "custom/calendar"
          "custom/power-menu"
        ];

        "wlr/workspaces" = {
          "format" = "{icon}";
          "on-scroll-up" = "hyprctl dispatch workspace e+1";
          "on-scroll-down" = "hyprctl dispatch workspace e-1";
          "on-click" = "activate";
          "all-outputs" = true;
        };

        "hyprland/window" = {
          "format" = "{}";
          "separate-outputs" = true;
        };

        "cpu" = {
          "interval" = 1;
          "format" = "<span font='Font Awesome 6 Pro' font_weight='medium' size='x-large' rise='-3pt'>&#xf2db;</span> <span font_weight='medium'>{usage}%</span>";
          "tooltip" = false;
          "on-click" = "$TERMINAL --class f_monitor -e bottom";
        };

        "temperature" = {
          "hwmon-path" = "/sys/class/hwmon/hwmon2/temp1_input";
          "critical-threshold" = 90;
          "interval" = 1;
          "format" = "<span font='Font Awesome 6 Pro' font_weight='medium' size='x-large' rise='-3pt'>&#xf06d;</span> <span font_weight='medium'>{temperatureC}°C</span>";
          "tooltip" = false;
          "on-click" = "$TERMINAL --class f_monitor -e bottom";
        };

        "clock" = {
          # "format" = "<span font_weight='medium'>{:%H:%M}</span>";
          "format" = "<span font='Font Awesome 6 Pro' font_weight='medium' size='x-large' rise='-3pt'>&#xf017;</span> <span font_weight='medium'>{:%H:%M}</span>";
          "min-length" = 4;
          "tooltip" = true;
          "tooltip-format" = "<span font='WenQuanYi Zen Hei Mono'><big>{:\t\t%Y %B \t week: %V }</big>\n<tt>{calendar}</tt></span>";
          "calendar" = {
            "mode"           = "year";
            "mode-mon-col"   = 3;
            "weeks-pos"      = "right";
            "on-scroll"      = 1;
            "on-click-right" = "mode";
            "format" = {
              "months"=     "<span color='#ffead3'><b>{}</b></span>"; # TODO: FIX COLORS with base16
              "days"=       "<span color='#ecc6d9'><b>{}</b></span>";
              "weeks"=      "<span color='#99ffdd'><b>W{}</b></span>";
              "weekdays"=   "<span color='#ffcc66'><b>{}</b></span>";
              "today"=      "<span color='#ff6699'><b><u>{}</u></b></span>";
            };
          };
        };

        # "custom/calendar" = {
        #   "tooltip" = true;
        #   "format"= "{}";
        #   "exec" = "$HOME/.config/waybar/cal.sh";
        #   "min-length" = 5;
        #   "return-type" = "json";
        # };


        "custom/calendar" = {
          "format" = "<span font='Font Awesome 6 Pro' font_weight='medium' size='x-large' rise='-3pt'>&#xf784;</span> <span font_weight='medium'>{}</span>";
          "exec" = "date +%d%a%b";
          "min-length" = 4;
          "tooltip" = true;
          "tooltip-format" = "<tt>{calendar}</tt>";
          "calendar" = {
            "mode"           = "year";
            "mode-mon-col"   = 3;
            "weeks-pos"      = "right";
            "on-scroll"      = 1;
            "on-click-right" = "mode";
            "format" = {
                      "months"=     "<span color='#ffead3'><b>{}</b></span>"; # TODO: FIX COLORS with base16
                      "days"=       "<span color='#ecc6d9'><b>{}</b></span>";
                      "weeks"=      "<span color='#99ffdd'><b>W{}</b></span>";
                      "weekdays"=   "<span color='#ffcc66'><b>{}</b></span>";
                      "today"=      "<span color='#ff6699'><b><u>{}</u></b></span>";
                      };
            };
        };

        "network" = {
          "interval" = 1;
          "format-ethernet" = "<span font='Font Awesome 6 Pro' font_weight='medium' size='x-large' rise='-3pt'>&#xf796;</span> {ifname}";
          "format-wifi" = "<span font='Font Awesome 6 Pro' font_weight='medium' size='large' rise='1pt'>&#xf1eb;</span>  <span rise='3pt'>{essid}</span>";
          "format-linked" = "<span font='Font Awesome 6 Pro' font_weight='medium' size='x-large' rise='-3pt'>&#xf0c1;</span> {essid} (No IP)";
          "format-disconnected" = "<span font='Font Awesome 6 Pro' font_weight='medium' size='x-large' rise='-3pt'>&#xf7a9;</span> No Internet";
          "format-disabled" = "<span font='Font Awesome 6 Pro' font_weight='medium' size='x-large' rise='-3pt'>&#xf7a9;</span> Networking Off";
          "on-click" = "network";
          "max-length" = 28;
          "tooltip" = false;
        };

        "pulseaudio" = {
          "format" = "<span font='Font Awesome 6 Pro' font_weight='medium' size='x-large' rise='-3pt'>{icon}</span><span font_weight='medium'> {volume}%</span>";
          "format-muted" = "<span font='Font Awesome 6 Pro' font_weight='medium' size='x-large' rise='-3pt'>&#xf6a9;</span> <span font_weight='medium'>{volume}%</span>";
          "format-icons" = {
            "default" = ["&#xf027;" "&#xf6a8;" "&#xf028;"];
          };
          "scroll-step" = 3;
          "on-click" = "wpctl set-mute @DEFAULT_SINK@ toggle";
          "on-click-right" = "$HOME/.config/waybar/sound_sink_toggle.sh";
          "on-click-middle" = "pavucontrol";
          "tooltip" = false;
          "min-length" = 8;
          "max-length" = 8;
        };

        "backlight" = {
          "format" = "<span font='Font Awesome 6 Pro' font_weight='medium' size='x-large' rise='-3pt'>{icon}</span> <span font_weight='medium'>  {percent}%</span>";
          "format-icons" = ["&#xe0ca" "&#xe0ca" "&#xe0c9"];
          "on-click" = "brightnessctl set 15%";
          "on-click-right" = "brightnessctl set 50%";
          "on-scroll-up" = "brightnessctl set +5%";
          "on-scroll-down" = "brightnessctl set 5%-";
          "min-length" = 8;
          "max-length" = 8;
        };

        "battery" = {
          "states" = {
            "warning" = 20;
            "critical" = 10;
          };
          "format" = "<span font='Font Awesome 6 Pro' font_weight='medium' size='x-large' rise='-3pt'>{icon}</span><span font_weight='medium'>  {capacity}%</span>";
          "format-icons" = ["&#xf377" "&#xf243" "&#xf242" "&#xf241" "&#xf240"];
          "format-charging" = "<span font='Font Awesome 6 Pro' font_weight='medium' size='x-large' rise='0pt'>&#xf376;</span> <span font_weight='medium' rise='2pt'>  {capacity}%</span>";
          "tooltip" = false;
          "min-length" = 8;
          "max-length" = 8;
        };

        "tray" = {
          "min-length" = 4;
          "icon-size" = 16;
          "tooltip" = false;
          "spacing" = 8;
        };

        "custom/bluetooth-battery" = {
          "format" = "<span font='Font Awesome 6 Pro' font_weight='medium' size='x-large' rise='-1pt'>&#xf025; </span><span font_weight='medium' rise='1pt'>{}</span>";
          "return-type" = "json";
          "interval" = 1;
          "exec" = "$HOME/.config/waybar/bluetooth-battery.sh";
          /* "exec" = "$HOME/.config/waybar/modules/bluetooth-battery-waybar-module.sh"; 
             "exec-if" = "bluetooth_battery_status.sh >/dev/null 2>&1"; */
          "signal" = 9;
          "on-click" = "blueman-applet && blueman-manager";
        };

        "custom/power-menu" = {
          "format" = "<span font='Font Awesome 6 Pro' size='x-large' font_weight='medium' rise='-3pt'>&#xf011;</span>";
          "tooltip" = false;
          "on-click" = "sh $HOME/.config/waybar/powermenu.sh";
        };

        "custom/start-menu" = {
          "format" = "<span font='Font Awesome 6 Pro' size='x-large' font_weight='medium' >&#xf043;</span>";
          "tooltip" = false;
          "min-length" = 5;
          "on-click" = "pkill -9 rofi || rofi -no-lazy-grab -show drun -theme $HOME/.config/rofi/config.rasi";
        };

        "custom/mpris-media" = { 
          "format" = "<span font='Font Awesome 6 Pro' size='x-large' font_weight='medium' rise='-2pt'>&#xf269;</span> {}";
          "return-type" = "json";
          "max-length" = 70;
          "exec" = "playerctl --player=io,com,spotify,firefox,%any metadata --format '{\"text\": \"{{artist}} - {{title}}\", \"tooltip\": \"{{playerName}} : {{artist}} - {{album}}{{markup_escape(title)}}\", \"alt\": \"{{playerName}}\", \"class\": \"{{playerName}}\"}' -F";
          "tooltip" = true;
          "on-click" = "$HOME/.config/waybar/get-media-player.sh";
        };

        "custom/song-next" = {
          "format" = "<span font='Font Awesome 6 Pro' size='large' font_weight='medium' rise='-3pt'>{icon}</span>";
          "on-click" = "playerctl next";
          "tooltip" = false;
          "min-length" = 1;
          "format-icons" = {
            "Playing" = "&#xf04e;";
            "Paused" = "&#xf04e;";
          };
          "exec" = "playerctl --player=com,spotify,firefox,%any metadata --format '{\"alt\": \"{{status}}\", \"class\": \"{{status}}\"}' -F";
          "return-type" = "json";
        };

        "custom/song-previous" = {
          "format" = "<span font='Font Awesome 6 Pro' size='large' font_weight='medium' rise='-3pt'>{icon}</span>";
          "on-click" = "playerctl previous";
          "min-length" = 1;
          "tooltip" = false;
          "format-icons" = {
            "Playing" = "&#xf04a;";
            "Paused" = "&#xf04a;";
          };
          "exec" = "playerctl --player=com,spotify,firefox,%any metadata --format '{\"alt\": \"{{status}}\", \"class\": \"{{status}}\"}' -F";
          "return-type" = "json";
        };

        "custom/song-toggle" = {
          "format" = "{icon}";
          "return-type" = "json";
          "tooltip" = true;
          "format-icons" = {
            "Paused" = "<span font='Font Awesome 6 Pro' rise='-2pt' font_weight='medium'>&#x2009;&#xf04b;</span>";
            "Playing" = "<span font='Font Awesome 6 Pro' rise='-2pt' size='large' font_weight='medium'>&#x2009;&#xf04c;</span>";
          };
          "exec" = "playerctl --player=io,com,spotify,firefox,%any metadata --format '{\"alt\": \"{{status}}\", \"tooltip\": \"{{artist}} - {{album}}{{markup_escape(title)}}\", \"class\": \"{{status}}\"}' -F";
          "on-click" = "playerctl play-pause";
        };

        "custom/recording" = {
          "format" = "{icon}";
          "return-type" = "json";
          "tooltip" = false;
          "format-icons" = {
            "Running" = "<span font='Font Awesome 6 Pro' rise='-2pt' size='large' font_weight='medium'>&#xe132;</span>";
          };
          "interval" = 1;
          /*"exec" = "$HOME/.config/waybar/recording.sh";
          "on-click" = "$HOME/.config/waybar/stop-recording.sh";*/
        };
      }
    ];
  };

}
