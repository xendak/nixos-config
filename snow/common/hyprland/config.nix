{ config, colorscheme }: ''

  
  #TRANSFORM

  general {
    max_fps=144
    gaps_in=5
    gaps_out=10
    border_size=2.7
    col.active_border=0xff${colorscheme.colors.base0C}
    col.inactive_border=0xff${colorscheme.colors.base02}
    resize_on_border=true
    hover_icon_on_border=false
  }

  decoration {
    active_opacity=1.0
    inactive_opacity=0.89
    fullscreen_opacity=1.0
    rounding=6
    blur {
      enabled=true
      size=5
      passes=3
      xray=true
      new_optimizations=true
      ignore_opacity=true
    }
    drop_shadow=true
    shadow_range=20
    shadow_offset=2 2
    col.shadow=0xba101010
    col.shadow_inactive=0x66101010
  }

  animations {
    enabled=true

    bezier=easein,0.11, 0, 0.5, 0
    bezier=easeout,0.5, 1, 0.89, 1
    bezier=easeinout,0.45, 0, 0.55, 1

    bezier=whoa,0.68,0.25,0.265,1.25
    bezier=smooth,0.445,0.05,0.55,0.95
    bezier=slow,0,0.85,0.3,1
    bezier=overshot,0.7,0.6,0.1,1.1

    animation=windowsIn,1,3,easeout,slide
    animation=windowsOut,1,3,easein,slide
    animation=windowsMove,1,0.5,easeout

    animation=fadeIn,1,3,easeout
    animation=fadeOut,1,3,easein
    animation=fadeSwitch,1,3,easeout
    animation=fadeShadow,1,3,easeout
    animation=fadeDim,1,3,easeout
    animation=border,1,3,easeout

    animation=workspaces,1,2,easeout,slide
  }

  dwindle {
    split_width_multiplier=1.35
  }

  misc {
    vfr=true
    vrr=1
    mouse_move_enables_dpms=true
  }

  input {
    kb_layout=us
    kb_variant=altgr-intl
    kb_options=ctrl:nocaps
    kb_rules=evdev
    follow_mouse=2
    touchpad {
      disable_while_typing=false
      natural_scroll=1
    }
  }

  # Rules

  windowrulev2 = size 1280 740,title:^(leagueclientux.exe)$
  windowrulev2 = size 1200 700,class:^(pavucontrol)$
  windowrulev2 = float,class:^(pavucontrol)$
  windowrulev2 = center,class:^(pavucontrol)$
  windowrulev2 = size 1280 740,title:$("League of Legends")$
  windowrulev2 = float,class:^(mpv)$
  windowrulev2 = float,title:^(Winetricks)(.*)$
  windowrulev2 = minsize 720 790,title:^(Winetricks)(.*)$
  windowrulev2 = float, class:^(moe.launcher.the-honkers-railway-launcher)$
  windowrulev2 = workspace special silent, class:^(moe.launcher.the-honkers-railway-launcher)$
  windowrulev2 = float,class:^(f_terminal)$
  windowrulev2 = float, class:^(moe.launcher.an-anime-game-launcher)$
  windowrulev2 = workspace special silent, class:^(moe.launcher.an-anime-game-launcher)$
  windowrulev2 = float,class:^(f_terminal)$
  windowrulev2 = size 1200 900,class:^(f_terminal)$
  windowrulev2 = float,class:^(org.gnome.Nautilus)$
  windowrulev2 = float,class:^(org.kde.dolphin)$
  windowrulev2 = size 1200 900,title:^(.*)(Home)(.*)$
  windowrulev2 = minsize 513 372,class:^(org.gnome.Nautilus)$,title:^(Merge Folder)$
  windowrulev2 = size 1350 900,title:^(Home)$
  windowrulev2 = float,class:^(.*)(exe)$,title:^(.*)(exe)$
  windowrulev2 = minsize 1200 700,class:^(.*exe)$,title:^(.*exe)$
  windowrulev2 = minsize 1200 700,class:^(.*bat)$,title:^(.*bat)$
  windowrulev2 = float, class:^(SteamTinkerLaunch)(.*)$
  windowrulev2 = minsize 1200 900, class:^(SteamTinkerLaunch)(.*)$
  windowrulev2 = float,class:^(.*)(bat)$,title:^(.*)(bat)$
  windowrulev2 = maximize,class:^(gamescope)$
  windowrulev2 = float,class:^(lutris)$
  windowrulev2 = minsize 1200 700,title:^(lutris)$
  windowrulev2 = float,title:^(Steam -)(.*)$
  windowrulev2 = float,title:^(Friends)(.*)$
  windowrulev2 = maximize,title:^(Terraria)(.*)$
  windowrulev2 = workspace 2 silent,title:^(.*).exe$
  windowrulev2 = workspace 6 silent,title:^(.*)(.bat)$ class:^(.*)(.bat)$
  windowrulev2 = workspace 4 silent,class:^(Steam)$
  windowrulev2 = workspace 4 silent,class:^(steam)$
  windowrulev2 = workspace 4 silent,class:^(lutris)$
  windowrulev2 = workspace 3 silent,class:^(discord)$
  windowrulev2 = opacity 1.0,class:^(mpv)$
  windowrulev2 = opacity 1.0,class:^(discord)$
  windowrulev2 = float, title:^(Picture-in-Picture)$
  windowrulev2 = pin, title:^(Picture-in-Picture)$
  windowrulev2 = float, title:^(Firefox — Sharing Indicator)$
  windowrulev2 = workspace special silent, title:^(Firefox — Sharing Indicator)$
  windowrulev2 = workspace special silent, title:^(.*is sharing (your screen|a window)\.)$
  windowrulev2 = move cursor 2070 515, title:^(Firefox — Sharing Indicator)$
  windowrulev2 = float,class:^(deluge)$
  windowrulev2 = forceinput,title:^(REDlauncher)$
  windowrulev2 = maximize,title:^(animegame - Wine desktop)$
  windowrulev2 = maximize,class:^(.gamescope-wrapped)$
  windowrulev2 = maximize,title:^(Honkai: Star Rail)$
  windowrulev2 = maximize,title:^(PHANTASY STAR ONLINE 2 NEW GENESIS)$
  # windowrulev2 = rouding:1, xwayland:1, floating:1
  # windowrulev2 = center, class:^(.*jetbrains.*)$, title:^(Confirm Exit|Open Project|win424|win201|splash)$
  layerrule = blur, ^(gtk-layer-shell)$
  layerrule = ignorezero, ^(gtk-layer-shell)$



  windowrule = nofocus,slurp

  # Startup
  exec-once=waybar
  exec-once=openrgb -d "XPG Spectrix S40G" -m Off
  #exec-once=xrandr --output XWAYLAND0 --primary # fix xwayland gaming issues
  exec-once=mkdir -p $HOME/Games/tmp/Screenshots
  exec-once=hyprctl setcursor '${config.gtk.cursorTheme.name}' 36
  exec-once=hyprpaper
  exec-once=mako
  exec-once=swayidle -w

  # Mouse binding
  bindm=SUPER,mouse:272,movewindow
  bindm=SUPERSHIFT,mouse:272,resizewindow
  bindm=SUPER,mouse:273,resizewindow

  # Program bindings
  bind=SUPER,Return,exec,$TERMINAL
  bind=SUPERSHIFT,Return,exec,$TERMINAL --class f_terminal
  bind=SUPERSHIFT,e,exec,$TERMINAL --class f_terminal -e $SHELL -ic '$TERMBROWSER -ndeiH'

  # bind=SUPER,w,exec,makoctl dismiss
  bind=SUPER,v,exec,$TERMINAL $SHELL -ic nvim
  bind=SUPERSHIFT,v,exec,$TERMINAL --class f_terminal -e $SHELL -ic nvim
  bind=SUPER,w,exec,$BROWSER
  bind=SUPER,e,exec,$FILEBROWSER
  bind=SUPER,r,exec,rofi -show drun -matching fuzzy -sorting-method fzf -sort -theme "$HOME/.config/rofi/config.rasi"

  bind=,Scroll_Lock,exec,pass-rofi # fn+k
  bind=,XF86Calculator,exec,pass-rofi # fn+f12

  # Some Random shit :)
  bind=SUPERCTRL,B,exec,wl-paste | base64 -d | wl-copy
  bind=SUPER,X,exec,sh "$HOME/.config/waybar/powermenu.sh"
  
  # powermenu submap
  bind=SUPER,X,submap,powermenu
  submap=powermenu

  binde=,r,exec,systemctl reboot
  binde=,d,exec,systemctl poweroff
  binde=,q,exec,systemctl pkill Hyprland
  binde=,p,exec,systemctl mpc -q pause && wpctl set-mute @DEFAULT_SINK@ toggle && systemctl suspend
  binde=,escape,exec,pkill rofi

  bind=,escape,submap,reset
  submap=reset

  

  # Toggle waybar
  bind=SUPERSHIFT,b,exec,pkill -USR1 waybar || waybar # profile button

  # Lock screen
  bind=,XF86Launch5,exec,swaylock -S
  bind=,XF86Launch4,exec,swaylock -S

  # Screenshots
  bind=,Print,exec,grimblast --notify copysave output "$HOME/Pictures/Screenshots/$(date +%Y-%m-%d-%M)_full.png"
  bind=SHIFT,Print,exec,grimblast --notify copysave active "$HOME/Games/tmp/Screenshots/$(date +%d-%M)_full.png"
  bind=ALTSHIFT,c,exec,grimblast --notify copysave area "$HOME/Games/tmp/Screenshots/$(date +%d-%M-%S)_snip.png"
  bind=ALTSHIFT,s,exec,grimblast --notify copysave area "$HOME/Pictures/Screenshots/Snips/$(date +%Y-%m-%d-%M-%S)_snip.png"

  # Keyboard controls (brightness, media, sound, etc)
  bind=,XF86MonBrightnessUp,exec,light -A 10
  bind=,XF86MonBrightnessDown,exec,light -U 10

  bind=,XF86AudioNext,exec,playerctl next
  bind=,XF86AudioPrev,exec,playerctl previous
  bind=,XF86AudioPlay,exec,playerctl play-pause
  bind=,XF86AudioStop,exec,playerctl stop
  bind=ALT,XF86AudioNext,exec,playerctld shift
  bind=ALT,XF86AudioPrev,exec,playerctld unshift
  bind=ALT,XF86AudioPlay,exec,systemctl --user restart playerctld
  bind=SUPER,XF86AudioPlay,exec,$TERMINAL $SHELL -ic lyrics

  bind=,123,exec,wpctl set-volume @DEFAULT_SINK@ +5%
  bind=,122,exec,wpctl set-volume @DEFAULT_SINK@ -5%
  bind=,XF86AudioMute,exec,wpctl set-mute @DEFAULT_SINK@ toggle
  bind=,XF86AudioMicMute,exec,wpctl set-mute @DEFAULT_SOURCE@ toggle

  # Window manager controls
  bind=SUPER,q,killactive

  bind=SUPER,s,togglesplit
  bind=SUPER,f,fullscreen,1
  bind=SUPERSHIFT,f,fullscreen,0
  bind=SUPERSHIFT,space,togglefloating

  bind=SUPER,minus,splitratio,-0.25
  bind=SUPERSHIFT,minus,splitratio,-0.3333333

  bind=SUPER,equal,splitratio,0.25
  bind=SUPERSHIFT,equal,splitratio,0.3333333

  bind=SUPER,g,togglegroup
  bind=SUPER,apostrophe,changegroupactive,f
  bind=SUPERSHIFT,apostrophe,changegroupactive,b

  bind=SUPER,left,movefocus,l
  bind=SUPER,right,movefocus,r
  bind=SUPER,up,movefocus,u
  bind=SUPER,down,movefocus,d
  bind=SUPER,h,movefocus,l
  bind=SUPER,l,movefocus,r
  bind=SUPER,k,movefocus,u
  bind=SUPER,j,movefocus,d

  bind=SUPERSHIFT,left,movewindow,l
  bind=SUPERSHIFT,right,movewindow,r
  bind=SUPERSHIFT,up,movewindow,u
  bind=SUPERSHIFT,down,movewindow,d
  bind=SUPERSHIFT,h,movewindow,l
  bind=SUPERSHIFT,l,movewindow,r
  bind=SUPERSHIFT,k,movewindow,u
  bind=SUPERSHIFT,j,movewindow,d

  bind=SUPERCONTROL,left,focusmonitor,l
  bind=SUPERCONTROL,right,focusmonitor,r
  bind=SUPERCONTROL,up,focusmonitor,u
  bind=SUPERCONTROL,down,focusmonitor,d
  bind=SUPERCONTROL,a,focusmonitor,l
  bind=SUPERCONTROL,d,focusmonitor,r
  bind=SUPERCONTROL,w,focusmonitor,u
  bind=SUPERCONTROL,s,focusmonitor,d

  bind=SUPERCONTROLSHIFT,left,movewindow,mon:l
  bind=SUPERCONTROLSHIFT,right,movewindow,mon:r
  bind=SUPERCONTROLSHIFT,up,movewindow,mon:u
  bind=SUPERCONTROLSHIFT,down,movewindow,mon:d
  bind=SUPERCONTROLSHIFT,h,movewindow,mon:l
  bind=SUPERCONTROLSHIFT,l,movewindow,mon:r
  bind=SUPERCONTROLSHIFT,k,movewindow,mon:u
  bind=SUPERCONTROLSHIFT,j,movewindow,mon:d

  bind=SUPERALT,left,movecurrentworkspacetomonitor,l
  bind=SUPERALT,right,movecurrentworkspacetomonitor,r
  bind=SUPERALT,up,movecurrentworkspacetomonitor,u
  bind=SUPERALT,down,movecurrentworkspacetomonitor,d
  bind=SUPERALT,h,movecurrentworkspacetomonitor,l
  bind=SUPERALT,l,movecurrentworkspacetomonitor,r
  bind=SUPERALT,k,movecurrentworkspacetomonitor,u
  bind=SUPERALT,j,movecurrentworkspacetomonitor,d

  bind=ALT,tab,togglespecialworkspace
  bind=ALTSHIFT,tab,movetoworkspace,special
  bind=WIN,tab,cyclenext
  bind=WIN,tab,bringactivetotop

  bind=SUPER,1,workspace,01
  bind=SUPER,2,workspace,02
  bind=SUPER,3,workspace,03
  bind=SUPER,4,workspace,04
  bind=SUPER,5,workspace,05
  bind=SUPER,6,workspace,06
  bind=SUPER,7,workspace,07
  bind=SUPER,8,workspace,08
  bind=SUPER,9,workspace,09
  bind=SUPER,0,workspace,10
  bind=SUPER,f1,workspace,11
  bind=SUPER,f2,workspace,12
  bind=SUPER,f3,workspace,13
  bind=SUPER,f4,workspace,14
  bind=SUPER,f5,workspace,15
  bind=SUPER,f6,workspace,16
  bind=SUPER,f7,workspace,17
  bind=SUPER,f8,workspace,18
  bind=SUPER,f9,workspace,19
  bind=SUPER,f10,workspace,20
  bind=SUPER,f11,workspace,21
  bind=SUPER,f12,workspace,22

  bind=SUPERSHIFT,1,movetoworkspacesilent,01
  bind=SUPERSHIFT,2,movetoworkspacesilent,02
  bind=SUPERSHIFT,3,movetoworkspacesilent,03
  bind=SUPERSHIFT,4,movetoworkspacesilent,04
  bind=SUPERSHIFT,5,movetoworkspacesilent,05
  bind=SUPERSHIFT,6,movetoworkspacesilent,06
  bind=SUPERSHIFT,7,movetoworkspacesilent,07
  bind=SUPERSHIFT,8,movetoworkspacesilent,08
  bind=SUPERSHIFT,9,movetoworkspacesilent,09
  bind=SUPERSHIFT,0,movetoworkspacesilent,10
  bind=SUPERSHIFT,f1,movetoworkspacesilent,11
  bind=SUPERSHIFT,f2,movetoworkspacesilent,12
  bind=SUPERSHIFT,f3,movetoworkspacesilent,13
  bind=SUPERSHIFT,f4,movetoworkspacesilent,14
  bind=SUPERSHIFT,f5,movetoworkspacesilent,15
  bind=SUPERSHIFT,f6,movetoworkspacesilent,16
  bind=SUPERSHIFT,f7,movetoworkspacesilent,17
  bind=SUPERSHIFT,f8,movetoworkspacesilent,18
  bind=SUPERSHIFT,f9,movetoworkspacesilent,19
  bind=SUPERSHIFT,f10,movetoworkspacesilent,20
  bind=SUPERSHIFT,f11,movetoworkspacesilent,21
  bind=SUPERSHIFT,f12,movetoworkspacesilent,22

''
