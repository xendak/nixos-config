{
  pkgs,
  config,
  ...
}:
let
  # TODO: remove this eventually
  colorscheme = import ../../colors/palettes/gorgoroth.nix;
  c = colorscheme.palette;

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
  im_height =
    if m.height == 1440 then
      "150"
    else if m.height == 1080 then
      "150"
    else
      "200";

  im_width =
    if m.height == 1440 then
      "40"
    else if m.height == 1080 then
      "40"
    else
      "200";
  wallpaper = "/persist/home/${config.home.username}/Flake/home/common/wallpapers/1.png";
  #wallpaper = "${config.home.wallpaper}";
in
{
  home.packages = with pkgs; [
    rofi # -unwrapped
  ];

  xdg.configFile."rofi/config.rasi".text = ''
    configuration {
      show-icons:                    true;
      icon-theme:                    "${config.gtk.iconTheme.name}";
      display-drun:                  "Applications";
      me-accept-entry:               "!MousePrimary";
      me-select-entry:               "MousePrimary";
      drun-display-format:           "{name}";
      disable-history:               false;
      hide-scrollbar:                false;
      sidebar-mode:                  false;
      font: "Sans 12";
    }

    window {
      location:	                     northwest;
      anchor:                        west;
      x-offset:                      10px;
      y-offset:                      10px;
      transparency:                  "real";
      width:                         20%;
      height:                        80%;
      border:                        2px;
      border-color:                  #${c.base0C};
      border-radius:                 10px;
      background-color:              #${c.base03};
    }

    entry {
      background-color:              #${c.base00};
      text-color:                    #${c.base05};
      placeholder-color:             #${c.base0C};
      expand:                        true;
      horizontal-align:              0.5;
      placeholder:                   " › Type here to search...";
      padding:                       10px;
      margin:                        0 1%;
      blink:                         true;
      border:                        0px 0px 2px 0px;
      border-radius:                 6px;
      border-color:                  #${c.base0D};
    }

    inputbar {
      children:                      [ entry ];
      background-image:		           url("${wallpaper}", width);
      border-radius:		             10px;
      border:                        0px 0px 2px 0px;
      border-color:                  #${c.base0D};
      text-color:                    #${c.base0B};
      expand:                        false;
      margin:                        5px 5px 6px 5px;
      padding:                       4%;
    }

    listview {
      background-color:              #${c.base00};
      border-radius:                 10px;
      padding:                       10px;
      columns:                       1;
      // lines:                         4;
      spacing:                       0%;
      cycle:                         true;
      dynamic:                       true;
      layout:                        vertical;
    }

    mainbox {
      background-color:              #${c.base00};
      border:                        0px;
      border-color:                  #${c.base0C};
      children:                      [ inputbar, listview ];
      spacing:                       0%;
      padding:                       8px;
    }

    element {
      orientation:                   horizontal;
      background-color:              #${c.base00};
      text-color:                    #${c.base05};
      border-radius:                 0%;
      padding:                       0px;
    }

    element-icon, element-text {
      background-color:              inherit;
      text-color:                    inherit;
    }

    element-icon {
      horizontal-align:              0.5;
      vertical-align:                0.5;
      size:                          42px;
      border:                        16px;
      border-color:                  transparent;
    }

    element-text {
      expand:                        true;
      horizontal-align:              0.1;
      vertical-align:                0.5;
      margin:                        10px 0px 10px 0px;
    }

    element selected {
      background-color:              #${c.base03};
      text-color:                    #${c.base0C};
      border:                        0px 0px 2px 0px;
      border-color:                  #${c.base0D};
      border-radius:                 10px;
    }
  '';

  xdg.configFile."rofi/powermenu.rasi".text = ''
    /**
     *
     * Author : Aditya Shakya (adi1090x)
     * Github : @adi1090x
     * EDITED BY: xendak
     *
     * Rofi Theme File
     * Rofi Version: 1.7.3
     **/

    /*****----- Configuration -----*****/
    configuration {
      show-icons:                 false;
      me-accept-entry:            "!MousePrimary";
      me-select-entry:            "MousePrimary";
    }

    /*****----- Global Properties -----*****/
    * {
        font:                        "Sans 16";
        background:                  #${c.base00};
        background-alt:              #${c.base03};
        foreground:                  #${c.base05};
        selected:                    #${c.base0D};
        active:                      #${c.base0C};
        urgent:                      #${c.base0D};
        button-style:                15px;
        border-style:                0px 0px 5px 0px;
    }

    /*****----- Main Window -----*****/
    window {
        transparency:                "real";
        location:                    center;
        anchor:                      center;
        fullscreen:                  true;
        x-offset:                    0px;
        y-offset:                    0px;
        padding:                     0px;
        border:                      0px solid;
        border-radius:               10px;
        border-color:                @selected;
        cursor:                      "default";
        background-color:            #${c.base03}60; // dim color
    }

    /*****----- Main Box -----*****/
    mainbox {
        enabled:                     true;
        spacing:                     0px;
        margin:                      ${fm_height}px ${fm_width}px; // Adjusted margins based on monitor height and width
        padding:                     0px;
        border:                      2px solid;
        border-radius:               16px;
        border-color:                @selected;
        background-color:            @background;
        children:                    [ "inputbar", "listview", "message" ];
    }

    /*****----- Inputbar -----*****/
    inputbar {
        enabled:                     true;
        spacing:                     0px;
        padding:                     ${im_height}px ${im_width}px; // Adjusted padding based on monitor height and width
        background-color:            transparent;
        background-image:            url("${wallpaper}", width);
        children:                    [ "textbox-prompt-colon", "dummy","prompt"];
    }

    dummy {
        background-color:            transparent;
    }

    textbox-prompt-colon {
        enabled:                     true;
        expand:                      false;
        str:                         "  System";
        padding:                     15px;
        border:                      @border-style;
        border-radius:               @button-style;
        border-color:                @selected;
        background-color:            @background-alt;
        text-color:                  @active;
    }
    prompt {
        enabled:                     true;
        padding:                     15px;
        border:                      @border-style;
        border-radius:               @button-style;
        border-color:                @selected;
        background-color:            @background-alt;
        text-color:                  @active;
    }

    /*****----- Listview -----*****/
    listview {
        enabled:                     true;
        columns:                     5;
        lines:                       1;
        cycle:                       true;
        dynamic:                     true;
        scrollbar:                   false;
        layout:                      vertical;
        reverse:                     false;
        fixed-height:                true;
        fixed-columns:               true;

        spacing:                     25px;
        margin:                      20px;
        background-color:            transparent;
        cursor:                      "default";
    }

    /*****----- Elements -----*****/
    element {
        enabled:                     true;
        padding:                     30px;
        border-radius:               @button-style;
        background-color:            @background-alt;
        text-color:                  @foreground;
        cursor:                      pointer;
    }
    element-text {
        font:                        "Font Awesome 6 Pro 54";
        background-color:            transparent;
        text-color:                  inherit;
        cursor:                      inherit;
        vertical-align:              0.5;
        horizontal-align:            0.5;
    }
    element selected.normal {
        border:                      @border-style;
        border-color:                @urgent;
        background-color:            var(background-alt);
        text-color:                  var(urgent);
    }

    /*****----- Message -----*****/
    message {
        enabled:                     true;
        margin:                      0px;
        padding:                     15px;
        border-radius:               0px;
        background-color:            @background-alt;
        text-color:                  @foreground;
    }
    textbox {
        background-color:            inherit;
        text-color:                  inherit;
        vertical-align:              0.5;
        horizontal-align:            0.5;
    }
  '';

  xdg.configFile."rofi/powermenu.sh".source = pkgs.writeShellScript "powermenu.sh" ''
    ## Author : Aditya Shakya (adi1090x)
    ## Github : @adi1090x

    # Current Theme
    theme="$HOME/.config/rofi/powermenu.rasi"

    # CMDs
    lastlogin="$(last "$USER" | head -n1 | tr -s ' ' | cut -d' ' -f5,6,7)"
    uptime="$(uptime | sed -e 's/up //g')"
    host=$(hostname)

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
        all-sync live-to-persist
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
}
