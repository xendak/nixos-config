{ config, pkgs, ... }:
let
  mkFcitx5Theme =
    { palette }:
    with palette;
    {
      themeConf = ''
        [Metadata]
        Name=current
        Version=0.1
        Author=thep0y
        Description="macOS modified for system colors."
        ScaleWithDPI=True

        [InputPanel]
        NormalColor=${base05}
        HighlightCandidateColor=${base02}
        EnableBlur=True
        BlurMask=
        FullWidthHighlight=True
        HighlightColor=${accent}
        HighlightBackgroundColor=#00000000
        PageButtonAlignment=Bottom

        [InputPanel/Background]
        Image=panel.svg

        [InputPanel/Background/Margin]
        Left=10
        Right=10
        Top=10
        Bottom=10

        [InputPanel/Highlight]
        Image=highlight.svg

        [InputPanel/Highlight/Margin]
        Left=20
        Right=20
        Top=8
        Bottom=8

        [InputPanel/TextMargin]
        Left=20
        Right=18
        Top=8
        Bottom=8

        [InputPanel/PrevPage]
        Image=

        [InputPanel/PrevPage/ClickMargin]
        Left=0
        Right=0
        Top=0
        Bottom=0

        [InputPanel/NextPage]
        Image=

        [Menu]
        NormalColor=${base02}
        HighlightCandidateColor=${base05}
        Spacing=0

        [Menu/Background]
        Image=
        Color=${base05}
        BorderColor=${base05}00
        BorderWidth=0
        Overlay=
        Gravity="Top Left"
        OverlayOffsetX=0
        OverlayOffsetY=0
        HideOverlayIfOversize=False

        [Menu/Background/Margin]
        Left=0
        Right=0
        Top=0
        Bottom=0

        [Menu/Background/OverlayClipMargin]
        Left=0
        Right=0
        Top=0
        Bottom=0

        [Menu/Highlight]
        Image=
        Color=${base05}
        BorderColor=${base05}00
        BorderWidth=0
        Overlay=
        Gravity="Top Left"
        OverlayOffsetX=0
        OverlayOffsetY=0
        HideOverlayIfOversize=False

        [Menu/Highlight/Margin]
        Left=0
        Right=0
        Top=0
        Bottom=0

        [Menu/Highlight/OverlayClipMargin]
        Left=0
        Right=0
        Top=0
        Bottom=0

        [Menu/Separator]
        Image=
        Color=${base05}
        BorderColor=${base05}00
        BorderWidth=0
        Overlay=
        Gravity="Top Left"
        OverlayOffsetX=0
        OverlayOffsetY=0
        HideOverlayIfOversize=False

        [Menu/Separator/Margin]
        Left=0
        Right=0
        Top=0
        Bottom=0

        [Menu/Separator/OverlayClipMargin]
        Left=0
        Right=0
        Top=0
        Bottom=0

        [Menu/CheckBox]
        Image=
        Color=${base05}
        BorderColor=${base05}00
        BorderWidth=0
        Overlay=
        Gravity="Top Left"
        OverlayOffsetX=0
        OverlayOffsetY=0
        HideOverlayIfOversize=False

        [Menu/CheckBox/Margin]
        Left=0
        Right=0
        Top=0
        Bottom=0

        [Menu/CheckBox/OverlayClipMargin]
        Left=0
        Right=0
        Top=0
        Bottom=0

        [Menu/SubMenu]
        Image=
        Color=${base05}
        BorderColor=${base05}00
        BorderWidth=0
        Overlay=
        Gravity="Top Left"
        OverlayOffsetX=0
        OverlayOffsetY=0
        HideOverlayIfOversize=False

        [Menu/SubMenu/Margin]
        Left=0
        Right=0
        Top=0
        Bottom=0

        [Menu/SubMenu/OverlayClipMargin]
        Left=0
        Right=0
        Top=0
        Bottom=0

        [Menu/ContentMargin]
        Left=0
        Right=0
        Top=0
        Bottom=0

        [Menu/TextMargin]
        Left=0
        Right=0
        Top=0
        Bottom=0
      '';

      highlightSvg = ''
        <svg
            xmlns="http://www.w3.org/2000/svg" width="39" height="39">
            <g id="Layer_1">
                <title>Layer 1</title>
                <rect id="svg_1" width="39" height="39" x="0" y="0" fill="${accent}" stroke="${base02}" stroke-width="0" rx="12"/>
            </g>
        </svg>
      '';

      panelSvg = ''
        <svg
            xmlns="http://www.w3.org/2000/svg" width="40" height="40">
            <g>
                <title>Layer 1</title>
                <rect id="svg_1" width="39" height="39" x=".5" y=".5" fill="${base02}" stroke="${base00}" rx="12"/>
            </g>
        </svg>
      '';
    };
in
{
  home.file = {
    # Dark theme
    ".config/themes/fcitx5-dark/theme.conf".text =
      (mkFcitx5Theme {
        palette = config.themes.dark.colorScheme.palette;
      }).themeConf;
    ".config/themes/fcitx5-dark/highlight.svg".text =
      (mkFcitx5Theme {
        palette = config.themes.dark.colorScheme.palette;
      }).highlightSvg;
    ".config/themes/fcitx5-dark/panel.svg".text =
      (mkFcitx5Theme {
        palette = config.themes.dark.colorScheme.palette;
      }).panelSvg;

    # Default theme
    ".config/themes/fcitx5-default/theme.conf".text =
      (mkFcitx5Theme {
        palette = config.themes.default.colorScheme.palette;
      }).themeConf;
    ".config/themes/fcitx5-default/highlight.svg".text =
      (mkFcitx5Theme {
        palette = config.themes.default.colorScheme.palette;
      }).highlightSvg;
    ".config/themes/fcitx5-default/panel.svg".text =
      (mkFcitx5Theme {
        palette = config.themes.default.colorScheme.palette;
      }).panelSvg;

    # Light theme
    ".config/themes/fcitx5-light/theme.conf".text =
      (mkFcitx5Theme {
        palette = config.themes.light.colorScheme.palette;
      }).themeConf;
    ".config/themes/fcitx5-light/highlight.svg".text =
      (mkFcitx5Theme {
        palette = config.themes.light.colorScheme.palette;
      }).highlightSvg;
    ".config/themes/fcitx5-light/panel.svg".text =
      (mkFcitx5Theme {
        palette = config.themes.light.colorScheme.palette;
      }).panelSvg;
  };

  i18n = {
    inputMethod = {
      enable = true;
      type = "fcitx5";
      fcitx5.addons = with pkgs; [
        fcitx5-mozc
        fcitx5-gtk
        # libsForQt5.fcitx5-qt
        kdePackages.fcitx5-qt
      ];
    };
  };

}
