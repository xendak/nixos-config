let 
  kbd_graphite = ''
    (def cfg
      process-unmaped-keys yes
    )

    (defsrc
      grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
      tab  q    w    e    r    t    y    u    i    o    p    [    ]    \
      caps a    s    d    f    g    h    j    k    l    ;    '    ret
      lsft z    x    c    v    b    n    m    ,    .    /    rsft
      lctl lmet lalt           spc           ralt rmet rctl)
    
    (deflayer qwerty
      grv @1q   2    3    4    5    6    7    8    9    0    -    =    bspc
      tab  q    w    e    r    t    y    u    i    o    p    [    ]    \
      @cap a    s    d    f    g    h    j    k    l    ;    '    ret
      lsft z    x    c    v    b    n    m    ,    .    /    rsft
      lctl lmet lalt           spc            ralt rmet rctl)
 
    (deflayer graphite
      grv @1g   2    3    4    5    6    7    8    9    0    -    =    bspc
      tab  q    w    e    r    t    y    u    i    o    p    [    ]    \
      @cap a    s    d    f    g    h    j    k    l    ;    '    ret
      lsft z    x    c    v    b    n    m    ,    .    /    rsft
      lctl lmet lalt           spc            ralt rmet rctl)

    (defalias
      ;; tap within 100ms for capslk, hold more than 100ms for lctl
      cap (tap-hold 100 100 bspc lctl)
      1g (tap-hold-release 100 100 @1 (layer-switch qwerty))
      1c (tap-hold-release 100 100 @1 (layer-switch graphite))
    )
  '';
in
{
  services.kanata = {
    enable = true;
    keyboards.laptop = {
      devices = ["/dev/input/by-path/platform-i8042-serio-0-event-kbd"];
      config = kbd_graphite;
    };
    keyboards.annepro2 = {
      devices = ["/dev/input/by-path/annepro2-kbd"];
      config = kbd_graphite;
    };
  };
}