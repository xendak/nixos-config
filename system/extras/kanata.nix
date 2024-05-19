let
  kbd_graphite = ''
    (defsrc
      grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
      tab  q    w    e    r    t    y    u    i    o    p    [    ]
      caps a    s    d    f    g    h    j    k    l    ;    '    ret
      lsft \    z    x    c    v    b    n    m    ,    .    /    rsft
      lctl lmet lalt           spc           ralt rmet rctl)

    (deflayer qwerty
    @grvg  1    2    3    4    5    6    7    8    9    0    -    =    bspc
      tab  q    w    e    r    t    y    u    i    o    p    [    ]
      @cap a    s    d    f    g    h    j    k    l    ;    '    ret
      lsft \    z    x    c    v    b    n    m    ,    .    /    rsft
      lctl lmet lalt           spc            ralt rmet rctl)

    (deflayer graphite
    @grvq  1    2    3    4    5    6    7    8    9    0    -    =    bspc
      tab  /    u    o    f    grv  z    w    d    l    q    [    ]
      '    i    e    a    h    y    g    s    t    r    n   ,    ret
      lsft x    -    .    p    k    v    c    m    j    b   ;    rsft
      lctl lmet lalt           spc            ralt rmet rctl)

    (defalias
      ;; tap within 100ms for capslk, hold more than 100ms for lctl
      cap (tap-hold 200 200 bspc lctl)
      grvq (tap-hold-release 200 200 grv (layer-switch qwerty))
      grvg (tap-hold-release 200 200 grv (layer-switch graphite))

    )
  '';
in {
  services.kanata = {
    enable = true;
    keyboards.laptop = {
      devices = ["/dev/input/by-path/platform-i8042-serio-0-event-kbd"];
      config = kbd_graphite;
    };
    # keyboards.annepro2 = {
    #   devices = ["/dev/input/by-id/usb-Obins_Anne_Pro_2_C18__QMK_-event-kbd"];
    #   config = kbd_graphite;
    # };
  };
}
