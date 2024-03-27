{ pkgs, lib, outputs, config, ... }:
let
  inherit (config.lib.formats.rasi) mkLiteral;
  inherit (config.colorscheme) colors;
  #inherit (config.wallpaper) wallpaper;
  wallpaper = "./rofi.png";
in
  {
    home.packages = with pkgs; [
      rofi-wayland #-unwrapped
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
        border-color:                  #${colors.base0C};
        border-radius:                 10px;
        background-color:              #${colors.base03};
      }

      entry {
        background-color:              #${colors.base00};
        text-color:                    #${colors.base05};
        placeholder-color:             #${colors.base0C};
        expand:                        true;
        horizontal-align:              0.5;
        placeholder:                   " › Type here to search...";
        padding:                       10px;
        margin:                        0 1%;
        blink:                         true;
        border:                        0px 0px 2px 0px;
        border-radius:                 6px;
        border-color:                  #${colors.base0D};
      }

      inputbar {
        children:                      [ entry ];
        background-image:		           url("${wallpaper}", width);
        border-radius:		             10px;
        border:                        0px 0px 2px 0px;
        border-color:                  #${colors.base0D};
        text-color:                    #${colors.base0B};
        expand:                        false;
        margin:                        5px 5px 6px 5px;
        padding:                       4%;
      }

      listview {
        background-color:              #${colors.base00};
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
        background-color:              #${colors.base00};
        border:                        0px;
        border-color:                  @border-color;
        children:                      [ inputbar, listview ];
        spacing:                       0%;
        padding:                       8px;
      }

      element {
        orientation:                   horizontal;
        background-color:              #${colors.base00};
        text-color:                    #${colors.base05};
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
        background-color:              #${colors.base03};
        text-color:                    #${colors.base0C};
        border:                        0px 0px 2px 0px;
        border-color:                  #${colors.base0D};
        border-radius:                 10px;
      }
    '';
    xdg.configFile."rofi/powermenu.rasi".text = ''
      /**
       *
       * Author : Aditya Shakya (adi1090x)
       * Github : @adi1090x
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
          background:                  #${colors.base00};
          background-alt:              #${colors.base03};
          foreground:                  #${colors.base05};
          selected:                    #${colors.base0D};
          active:                      #${colors.base0C};
          urgent:                      #${colors.base0D};
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
          background-color:            #${colors.base03}60; // dim color
      }

      /*****----- Main Box -----*****/
      mainbox {
          enabled:                     true;
          spacing:                     0px;
          margin:                      398px 600px;
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
          padding:                     150px 40px;
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
          str:                         " System";
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
  }
