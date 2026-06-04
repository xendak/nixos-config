{
  paletteSet,
  config,
  pkgs,
  ...
}:
let
  p = paletteSet.palette;
  f = config.fontProfiles;

in
{
  "dunst/dunstrc" = ''
    [global]
        ### Display ###
        monitor = 0
        follow = mouse

        ### Geometry ###
        width = (0, 700)
        height = 120
        origin = top-right
        offset = 20x20
        scale = 0
        notification_limit = 5

        ### Progress bar ###
        progress_bar = true
        progress_bar_height = 10
        progress_bar_frame_width = 1
        progress_bar_min_width = 150
        progress_bar_max_width = 300

        indicate_hidden = yes
        transparency = 0
        separator_height = 0
        padding = 22
        horizontal_padding = 22
        text_icon_padding = 0
        frame_width = 2
        
        # Using palette outline/surface for the frame
        frame_color = "${p.primary}"
        separator_color = foreground
        sort = yes

        ### Text ###
        font = ${f.regular.family} 14
        line_height = 10
        markup = full

        # The format string with palette-aware spans
        format = "<span rise='-4000' font_desc='icomoon 13' foreground='${p.primary}'></span>  <span foreground='${p.on_surface}'>%s</span>\n<span foreground='${p.outline}' rise='-4000' font_desc='icomoon 13'></span>  <span foreground='${p.on_surface}'>%b</span>"

        alignment = left
        vertical_alignment = center
        show_age_threshold = 60
        ellipsize = middle
        ignore_newline = yes
        stack_duplicates = true
        hide_duplicate_count = false
        show_indicators = false

        ### Icons ###
        icon_position = off
        min_icon_size = 36
        max_icon_size = 36
        icon_path = "${config.gtk.iconTheme.package}/share/icons/${config.gtk.iconTheme.name}/48x48/actions:${config.gtk.iconTheme.package}/share/icons/${config.gtk.iconTheme.name}/48x48/apps:${config.gtk.iconTheme.package}/share/icons/${config.gtk.iconTheme.name}/48x48/devices:${config.gtk.iconTheme.package}/share/icons/${config.gtk.iconTheme.name}/48x48/emblems:${config.gtk.iconTheme.package}/share/icons/${config.gtk.iconTheme.name}/48x48/emotes:${config.gtk.iconTheme.package}/share/icons/${config.gtk.iconTheme.name}/48x48/mimetypes:${config.gtk.iconTheme.package}/share/icons/${config.gtk.iconTheme.name}/48x48/places:${config.gtk.iconTheme.package}/share/icons/${config.gtk.iconTheme.name}/48x48/status"
        ### History ###
        sticky_history = yes
        history_length = 20

        ### Misc/Advanced ###
        dmenu = "${pkgs.rofi}/bin/rofi -dmenu -p dunst:"
        browser = "${pkgs.handlr-regex}/bin/handlr open"
        always_run_script = true
        title = Dunst
        class = Dunst
        corner_radius = 12
        ignore_dbusclose = false

        ### mouse ###
        mouse_left_click = close_current
        mouse_middle_click = do_action, close_current
        mouse_right_click = close_all

    [urgency_low]
        background = "${p.surface_container_low}"
        foreground = "${p.on_surface}"
        frame_color = "${p.surface_container_high}"
        timeout = 2

    [urgency_normal]
        background = "${p.surface_container_low}"
        foreground = "${p.on_surface}"
        frame_color = "${p.primary}"
        timeout = 3

    [urgency_critical]
        background = "${p.error}"
        foreground = "${p.on_error}"
        frame_color = "${p.surface_container_high}"
        timeout = 0
  '';
}
