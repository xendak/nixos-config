import { type BarWidget } from "widget/bar/Bar"
import { opt, mkOptions } from "lib/option"
import { distro } from "lib/variables"
import { icon } from "lib/utils"
import icons from "lib/icons"

const options = mkOptions(OPTIONS, {
    autotheme: opt(false),

    wallpaper: opt(`/persist/home/drops/Flake/home/common/wallpapers/1.png`, { persistent: true }),

    theme: {
        dark: {
            primary: {
                bg: opt("#ea6962"),
                fg: opt("#141414"),
            },
            error: {
                bg: opt("#e55f86"),
                fg: opt("#141414"),
            },
            bg: opt("#171717"),
            fg: opt("#eeeeee"),
            widget: opt("#eeeeee"),
            border: opt("#eeeeee"),
        },
        light: {
            primary: {
                bg: opt("#ea6962"),
                fg: opt("#eeeeee"),
            },
            error: {
                bg: opt("#b13558"),
                fg: opt("#eeeeee"),
            },
            bg: opt("#fffffa"),
            fg: opt("#080808"),
            widget: opt("#080808"),
            border: opt("#080808"),
        },

        blur: opt(0),
        scheme: opt<"dark" | "light">("dark"),
        widget: { opacity: opt(94) },
        border: {
            width: opt(1),
            opacity: opt(96),
        },

        shadows: opt(true),
        padding: opt(7),
        spacing: opt(12),
        radius: opt(11),
    },

    transition: opt(200),

    font: {
        size: opt(13),
        name: opt("Sofia Pro"),
    },

    bar: {
        flatButtons: opt(true),
        position: opt<"top" | "bottom">("top"),
        corners: opt(true),
        layout: {
            start: opt<BarWidget[]>([
                "launcher",
                "workspaces",
                "taskbar",
                "expander",
                "messages",
            ]),
            center: opt<BarWidget[]>([
                "date",
            ]),
            end: opt<BarWidget[]>([
                "media",
                "expander",
                "systray",
                "colorpicker",
                "screenrecord",
                "system",
                "battery",
                "powermenu",
            ]),
        },
        launcher: {
            icon: {
                colored: opt(true),
                icon: opt(icon(icons.ui.nixos, icons.ui.search)),
            },
            label: {
                colored: opt(false),
                label: opt(""),
            },
            action: opt(() => App.toggleWindow("applauncher")),
        },
        date: {
            format: opt("%H:%M - %a%e"),
            action: opt(() => App.toggleWindow("datemenu")),
        },
        battery: {
            bar: opt<"hidden" | "regular" | "whole">("whole"),
            charging: opt("#00D787"),
            percentage: opt(true),
            blocks: opt(7),
            width: opt(50),
            low: opt(30),
        },
        workspaces: {
            workspaces: opt(5),
        },
        taskbar: {
            iconSize: opt(0),
            monochrome: opt(false),
            exclusive: opt(false),
        },
        messages: {
            action: opt(() => App.toggleWindow("datemenu")),
        },
        systray: {
            ignore: opt([
                "KDE Connect Indicator",
                "spotify-client",
                "udiskie",
            ]),
        },
        media: {
            monochrome: opt(true),
            preferred: opt("playerctl"),
            direction: opt<"left" | "right">("right"),
            length: opt(40),
        },
        powermenu: {
            monochrome: opt(false),
            action: opt(() => App.toggleWindow("powermenu")),
        },
    },

    applauncher: {
        iconSize: opt(64),
        width: opt(0),
        margin: opt(80),
        maxItem: opt(6),
        favorites: opt([
            [
                "firefox",
                "org.gnome.Nautilus",
                "org.gnome.Calendar",
                "obsidian",
                "discord",
                "kitty",
                "org.kde.dolphin",
            ],
        ]),
    },

    overview: {
        scale: opt(9),
        workspaces: opt(5),
        monochromeIcon: opt(true),
    },

    powermenu: {
        sleep: opt("systemctl suspend"),
        reboot: opt("systemctl reboot"),
        logout: opt("pkill Hyprland"),
        shutdown: opt("shutdown now"),
        layout: opt<"line" | "box">("line"),
        labels: opt(true),
    },

    quicksettings: {
        avatar: {
            image: opt(`/persist/home/drops/Flake/home/drops/profile.png`),
            size: opt(70),
        },
        width: opt(380),
        position: opt<"left" | "center" | "right">("right"),
        networkSettings: opt("gtk-launch gnome-control-center"),
        media: {
            monochromeIcon: opt(true),
            coverSize: opt(100),
        },
    },

    datemenu: {
        position: opt<"left" | "center" | "right">("center"),
    },

    osd: {
        progress: {
            vertical: opt(true),
            pack: {
                h: opt<"start" | "center" | "end">("end"),
                v: opt<"start" | "center" | "end">("center"),
            },
        },
        microphone: {
            pack: {
                h: opt<"start" | "center" | "end">("center"),
                v: opt<"start" | "center" | "end">("end"),
            },
        },
    },

    notifications: {
        position: opt<Array<"top" | "bottom" | "left" | "right">>(["top", "right"]),
        blacklist: opt(["Spotify"]),
        width: opt(440),
    },

    hyprland: {
        gaps: opt(2.4),
        inactiveBorder: opt("333333ff"),
    },
})

globalThis["options"] = options
export default options
