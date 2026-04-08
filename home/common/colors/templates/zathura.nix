{ paletteSet, config, ... }:
let
  m = paletteSet.palette;
in
{
  "zathura/zathurarc" = ''
    set selection-clipboard "clipboard"
    set font "${config.fontProfiles.regular.family} 14"
    set recolor "true"

    ; Surfaces
    set default-bg       "${m.background}"
    set default-fg       "${m.on_surface}"
    set statusbar-bg     "${m.surface_container_high}"
    set statusbar-fg     "${m.fg}"
    set inputbar-bg      "${m.surface_container}"
    set inputbar-fg      "${m.on_surface}"

    ; Highlight (search results) → primary container
    set highlight-color         "${m.blue}"
    set highlight-active-color  "${m.selection_bg}"
    set highlight-fg            "${m.selection_fg}"

    ; Recolor (for document pages)
    set recolor-lightcolor  "${m.background}"
    set recolor-darkcolor   "${m.on_surface}"

    ; Completion popup
    set completion-bg     "${m.surface_container_high}"
    set completion-fg     "${m.on_surface}"
    set completion-highlight-bg  "${m.primary_container}"
    set completion-highlight-fg  "${m.on_primary_container}"

    ; Notifications
    set notification-bg          "${m.surface_container_high}"
    set notification-fg          "${m.on_surface}"
    set notification-error-bg    "${m.error_container}"
    set notification-error-fg    "${m.on_error_container}"
    set notification-warning-bg  "${m.tertiary_container}"
    set notification-warning-fg  "${m.on_tertiary_container}"
  '';
}
