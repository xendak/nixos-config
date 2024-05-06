let 
  kbd_graphite = ''
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
      tab  /    u    o    f    grv  z    w    d    l    q    [    ]    \
      @cap i   e    a    h    y    g    s    t    r    n   ,    ret
      lsft x    -    .    p    k    v    c    m    j    b    rsft
      lctl lmet lalt           spc            ralt rmet rctl)

    (deflayer no-mod-graphite
      grv @1g   2    3    4    5    6    7    8    9    0    -    =    bspc
      tab  /    u    o    f    grv  z    w    d    l    q    [    ]    \
      @cap i    e    a    h    y    g    s    t    r    n    ,    ret
      lsft x    -    .    p    k    v    c    m    j    b    rsft
      lctl lmet lalt           spc            ralt rmet rctl)

    (defvar
      ;; Note: consider using different time values for your different fingers.
      ;; For example, your pinkies might be slower to release keys and index
      ;; fingers faster.
      tap-time 200
      hold-time 150

      left-hand-keys (
        / u o f grv
        i e a h y
        x - . p k
      )
      right-hand-keys (
        z w d l q
        g s t r n
        v c m j b
      )
    )

    (deffakekeys
      to-base (layer-switch graphite)
    )
    
    (defalias
      ;; tap within 100ms for capslk, hold more than 100ms for lctl
      cap (tap-hold 150 150 bspc lctl)
      1g (tap-hold-release 150 150 1 (layer-switch qwerty))
      1q (tap-hold-release 150 150 1 (layer-switch graphite))

      tap (multi
          (layer-switch no-mod-graphite)
          (on-idle-fakekey to-base tap 20)
      )
      i (tap-hold-release-keys $tap-time $hold-time (multi i @tap) lsft $left-hand-keys)
      n (tap-hold-release-keys $tap-time $hold-time (multi n @tap) rsft $right-hand-keys)
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
