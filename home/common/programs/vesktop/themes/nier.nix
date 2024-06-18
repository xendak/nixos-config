{pkgs, ...}: {
  xdg.configFile."vesktop/settings/quickCss.css".source =
    pkgs.writeText "quickCss.css"
    ''
      @import url('https://accrazed.github.io/YoRHA-UI-BetterDiscord/font/stylesheet.css'); /* Font */
      @import url('https://accrazed.github.io/YoRHA-UI-BetterDiscord/NieR-Cursor.css'); /* Cursor */
      @import url('https://accrazed.github.io/YoRHA-UI-BetterDiscord/NieR-Import.theme.css'); /* Main File */
      @import url('https://accrazed.github.io/YoRHA-UI-BetterDiscord/LightImport.theme.css'); /* Light Part of Theme */

      @import url('https://buns.gay/YoRHA-UI-BetterDiscord/font/stylesheet.css'); /* Font */
      @import url('https://buns.gay/YoRHA-UI-BetterDiscord/NieR-Cursor.css'); /* Cursor */
      @import url('https://buns.gay/YoRHA-UI-BetterDiscord/NieR-Import.theme.css'); /* Main File */
      @import url('https://buns.gay/YoRHA-UI-BetterDiscord/LightImport.theme.css'); /* Light Part of Theme */

      /* GLOBAL VARIABLES */
      :root {
      	--color-high: rgb(218, 212, 187);
      	--color-low: rgb(87, 84, 74);
      	--color-mid: rgb(151, 147, 129);
      	--color-alert: rgb(205, 102, 77);
      	--color-high-trans: rgba(218, 212, 187, 0.8);
      	--color-low-trans: rgba(87, 84, 74, 0.6);
      	--color-alert-trans: rgba(205, 102, 77, 0.7);
      	--nier-font: sce-ps3_rodin_latinregular;
           }
    '';
}
