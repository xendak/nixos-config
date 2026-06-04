{
  paletteSet,
  config,
  ...
}:
let
  p = paletteSet.palette;

  m = builtins.elemAt config.monitors 0;
  hStr = toString m.height;

  # Resolution-to-Size Mapping
  resSettings = {
    "1440" = {
      fm = {
        h = "398";
        w = "600";
      };
      im = {
        h = "150";
        w = "40";
      };
      fs = {
        n = "12";
        h = "16";
        i = "56";
      };
    };
    "1080" = {
      fm = {
        h = "247";
        w = "350";
      };
      im = {
        h = "150";
        w = "40";
      };
      fs = {
        n = "12";
        h = "16";
        i = "56";
      };
    };
    "720" = {
      fm = {
        h = "150";
        w = "100";
      };
      im = {
        h = "100";
        w = "90";
      };
      fs = {
        n = "10";
        h = "12";
        i = "36";
      };
    };
  };

  fallback = {
    fm = {
      h = "100";
      w = "200";
    };
    im = {
      h = "80";
      w = "20";
    };
    fs = {
      n = "10";
      h = "12";
      i = "36";
    };
  };

  # Select the current config based on monitor height
  cfg = resSettings.${hStr} or fallback;

  # Extract for easier use in the RASI strings
  fm = cfg.fm;
  im = cfg.im;
  font = cfg.fs;

  wallpaper = "/home/${config.home.username}/.local/state/caelestia/wallpaper/current";
in
{
  "rofi/config.rasi" = ''
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
      font:                          "Sans ${font.n}";
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
      border-color:                  ${p.primary};
      border-radius:                 10px;
      background-color:              ${p.surface_container};
    }

    entry {
      background-color:              ${p.bg};
      text-color:                    ${p.fg};
      placeholder-color:             ${p.primary};
      expand:                        true;
      horizontal-align:              0.5;
      placeholder:                   " › Type here to search...";
      padding:                       10px;
      margin:                        0 1%;
      blink:                         true;
      border:                        0px 0px 2px 0px;
      border-radius:                 6px;
      border-color:                  ${p.primary};
    }

    inputbar {
      children:                      [ entry ];
      background-image:		           url("${wallpaper}", width);
      border-radius:		             10px;
      border:                        0px 0px 2px 0px;
      border-color:                  ${p.primary};
      text-color:                    ${p.primary}; // maybe not this?
      expand:                        false;
      margin:                        5px 5px 6px 5px;
      padding:                       4%;
    }

    listview {
      background-color:              ${p.bg};
      border-radius:                 10px;
      padding:                       10px;
      columns:                       1;
      spacing:                       8px;
      cycle:                         true;
      dynamic:                       true;
      layout:                        vertical;
    }

    mainbox {
      background-color:              ${p.bg};
      border:                        0px;
      border-color:                  ${p.primary};
      children:                      [ inputbar, listview ];
      spacing:                       0%;
      padding:                       8px;
    }

    element {
      orientation:                   horizontal;
      background-color:              ${p.bg};
      text-color:                    ${p.fg};
      border-radius:                 10px;
      padding:                       0px;
    }

    element normal.normal {
      background-color: ${p.surface_container};
      text-color: ${p.fg};
    }

    element alternate.normal {
      background-color: ${p.surface_container_high};
      text-color: ${p.fg};
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
      background-color:              ${p.surface_container};
      text-color:                    ${p.primary};
      border:                        0px 0px 2px 0px;
      border-color:                  ${p.primary};
      border-radius:                 10px;
    }
  '';

  "rofi/fullscreen-preview.rasi" = ''
    configuration {
        modi:                       "filebrowser";
        show-icons:                 true;

    }

    * {
        background:                  ${p.background}70;
        background-alt:              ${p.surface_container};
        foreground:                  ${p.foreground};
        selected:                    ${p.primary};
        active:                      ${p.primary};
        urgent:                      ${p.primary};

        font:                        "Sans 12";
        element-padding:             6px;
        element-spacing:             10px;

      	box-spacing:                 40px;
        background-color:            transparent;
        text-color:                  @foreground;
    }

    mainbox {
        enabled:                     true;
        spacing:                     @box-spacing;
        background-color:            transparent;
        children:                    [ wrap, listview-split ];

        margin:                      10% 5% 10% 5%;
        padding:                     0px;
    }

    window {
        transparency:                "real";
        fullscreen:                  true;
        background-color:            @background;
        padding:                     3em;
        children:                    [ wrap, listview-split ];
        spacing:                     2em;

    }

    icon-current-entry {
        expand:                      true;
        size:                        100%;
    }

    listview-split {
        orientation:                 horizontal;
        spacing:                     1.5em;
        children:                    [ listview ];
    	expand: 					 true;
    }

    wrap {
        expand:                      false;
        orientation:                 vertical;
        children:                    [ inputbar ];
        background-color:            @selected;
        border-color:                @selected;
        border:                      2px;
        border-radius:               100px;
    }

    icon-ib {
        expand:                      false;
        filename:                    "system-search";
        vertical-align:              0.5;
        horizontal-align:            0.5;
        size:                        1.2em;
        text-color:                  @selected;
      	vertical-align:              0.5;
        horizontal-align:            0.5;

        margin:                      0px 0px 0px 20px;
    }

    inputbar {
        spacing:                     0.5em;
        padding:                     0.6em;
        children:                    [ icon-ib, entry ];
    }

    entry {
        placeholder:                 "";
        placeholder-color:           @background;
        text-color:                  @background;
    	  vertical-align:              0.7;
        horizontal-align:            0.0;

        padding:                     0px 20px 0px 0px;
      	cursor:                      text;
    }

    message {
        background-color:            #ff000020;
        border-color:                #ffa07a;
        border:                      2px 0px 0px 0px;
        padding:                     0.4em;
        spacing:                     0.4em;
    }

    listview {
        flow:                        horizontal;
        fixed-columns:               true;
        columns:                     8;
        lines:                       5;
        spacing:                     1.2em;
        fixed-height:                false;
      	expand: 					           true;

    }

    element {
        orientation:                 vertical;
        padding:                     @element-padding;
        spacing:                     @element-spacing;
        background-color:            @background-alt;
        border-color:                @background-alt;
        border:                      2px;
        border-radius:               20px;
    }

    element normal.normal,
    element alternate.normal {
        background-color:            @background-alt;
        text-color:                  @foreground;
    }

    element-icon {
        size:                        16em;
        horizontal-align:            0.5;
        vertical-align:              0.5;
        background-color:            transparent;
    }

    element-text {
        horizontal-align:            0.5;
        vertical-align:              0.5;
        padding:                     0.1em 0.2em;
        text-color:                  inherit;
    }

    element selected.normal,
    element selected {
        background-color:            @background-alt;
        border-color:                @selected;
        border:                      0px 0px 5px 0px;
        text-color:                  @selected;
    }

    @media ( enabled: env(PREVIEW, false)) {
        listview-split {
            children:                [ listview, icon-current-entry ];
        }
        listview {
            columns:                 4;
        }
    }

    @media ( enabled: env(NO_IMAGE, false)) {
        listview {
            columns:                 1;
            spacing:                 0.4em;
        }
        element {
            children:                [ element-text ];
        }
        element-text {
            horizontal-align:        0.0;
        }
    }
  '';

  "rofi/powermenu.rasi" = ''
      /**
       *
       * Author : Aditya Shakya (adi1090x)
       * Github : @adi1090x
       * EDITED BY: xendak
       *
       * Rofi Theme File
       * Rofi Version: 1.7.3
     **/

    configuration {
      show-icons:                 false;
      me-accept-entry:            "!MousePrimary";
      me-select-entry:            "MousePrimary";
    }

    * {
        font:                        "Sans ${font.h}";
        background:                  ${p.bg};
        background-alt:              ${p.surface_container};
        foreground:                  ${p.fg};
        selected:                    ${p.primary};
        active:                      ${p.primary};
        urgent:                      ${p.primary};
        button-style:                15px;
        border-style:                0px 0px 5px 0px;
    }

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
        background-color:            ${p.surface_container}60; // dim color
    }

    mainbox {
        enabled:                     true;
        spacing:                     0px;
        margin:                      ${fm.h}px ${fm.w}px;
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
        padding:                     ${im.h}px ${im.w}px;
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

    element {
        enabled:                     true;
        padding:                     30px;
        border-radius:               @button-style;
        background-color:            @background-alt;
        text-color:                  @foreground;
        cursor:                      pointer;
    }

    element-text {
        font:                        "Font Awesome 6 Pro ${font.i}";
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
}
