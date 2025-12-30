pragma Singleton

import Quickshell
import Quickshell.Io

Singleton {
    id: root

    readonly property var osIcons: ({
            almalinux: "",
            alpine: "",
            arch: "",
            archcraft: "",
            arcolinux: "",
            artix: "",
            centos: "",
            debian: "",
            devuan: "",
            elementary: "",
            endeavouros: "",
            fedora: "",
            freebsd: "",
            garuda: "",
            gentoo: "",
            hyperbola: "",
            kali: "",
            linuxmint: "󰣭",
            mageia: "",
            openmandriva: "",
            manjaro: "",
            neon: "",
            nixos: "",
            opensuse: "",
            suse: "",
            sles: "",
            sles_sap: "",
            "opensuse-tumbleweed": "",
            parrot: "",
            pop: "",
            raspbian: "",
            rhel: "",
            rocky: "",
            slackware: "",
            solus: "",
            steamos: "",
            tails: "",
            trisquel: "",
            ubuntu: "",
            vanilla: "",
            void: "",
            zorin: ""
        })

    readonly property var weatherIcons: ({
            "113": "clear_day",
            "116": "partly_cloudy_day",
            "119": "cloud",
            "122": "cloud",
            "143": "foggy",
            "176": "rainy",
            "179": "rainy",
            "182": "rainy",
            "185": "rainy",
            "200": "thunderstorm",
            "227": "cloudy_snowing",
            "230": "snowing_heavy",
            "248": "foggy",
            "260": "foggy",
            "263": "rainy",
            "266": "rainy",
            "281": "rainy",
            "284": "rainy",
            "293": "rainy",
            "296": "rainy",
            "299": "rainy",
            "302": "weather_hail",
            "305": "rainy",
            "308": "weather_hail",
            "311": "rainy",
            "314": "rainy",
            "317": "rainy",
            "320": "cloudy_snowing",
            "323": "cloudy_snowing",
            "326": "cloudy_snowing",
            "329": "snowing_heavy",
            "332": "snowing_heavy",
            "335": "snowing",
            "338": "snowing_heavy",
            "350": "rainy",
            "353": "rainy",
            "356": "rainy",
            "359": "weather_hail",
            "362": "rainy",
            "365": "rainy",
            "368": "cloudy_snowing",
            "371": "snowing",
            "374": "rainy",
            "377": "rainy",
            "386": "thunderstorm",
            "389": "thunderstorm",
            "392": "thunderstorm",
            "395": "snowing"
        })

    // this checks a non .desktop into a .desktop
    readonly property var desktopEntrySubs: ({
        "zen": "zen-browser",
        "zen-beta": "zen-browser",
        "f_terminal": "org.wezfurlong.wezterm",
        "f_yazi": "yazi",
        "nu": "org.wezfurlong.wezterm",
        "btm": "utilities-system-monitor",
        "bottom": "utilities-system-monitor",
    })

    // this checks inside home/common/icons
    readonly property var customIconEntrySubs: ({
        "btm": "bottom.svg",
        "org.pwmt.zathura": "zathura.svg",
        "vesktop": "vesktop.png",
        "imv": "imv.svg",
        // "bottom": "bottom.svg",
    })

    readonly property var categoryIcons: ({
            WebBrowser: "web",
            Printing: "print",
            Security: "security",
            Network: "chat",
            Archiving: "archive",
            Compression: "archive",
            Development: "code",
            IDE: "code",
            TextEditor: "edit_note",
            Audio: "music_note",
            Music: "music_note",
            Player: "music_note",
            Recorder: "mic",
            Game: "sports_esports",
            FileTools: "folder",
            FileManager: "folder",
            Filesystem: "folder",
            FileTransfer: "folder",
            Settings: "settings",
            DesktopSettings: "settings",
            HardwareSettings: "settings",
            TerminalEmulator: "terminal",
            ConsoleOnly: "terminal",
            Utility: "build",
            Monitor: "monitor_heart",
            Midi: "graphic_eq",
            Mixer: "graphic_eq",
            AudioVideoEditing: "video_settings",
            AudioVideo: "music_video",
            Video: "videocam",
            Building: "construction",
            Graphics: "photo_library",
            "2DGraphics": "photo_library",
            RasterGraphics: "photo_library",
            TV: "tv",
            System: "host",
            Office: "content_paste"
        })

    property string osIcon: ""
    property string osName

    function resolveIconPath(name: string, fallback: string): string {
        if (!name && !fallback) fallback = "application-default-icon";
        if (!name) {
            return Quickshell.iconPath(fallback);
        }
        
        if (desktopEntrySubs.hasOwnProperty(name)) {
            name = desktopEntrySubs[name];
        }
        
        if (customIconEntrySubs.hasOwnProperty(name)) {
            const customFile = customIconEntrySubs[name];
            const customIconPath = "file:/" + Quickshell.env("HOME") + "/Flake/home/common/icons/" + customFile;
            return customIconPath;
        }
        
        return Quickshell.iconPath(name, fallback);
    }

    function getDesktopEntry(name: string): DesktopEntry {
        if (!name) return null;
        
        // original name
        let entry = DesktopEntries.applications.values.find(a => a.id.toLowerCase() === name.toLowerCase());
        if (entry) return entry;
        
        // lowercase and dashes
        name = name.toLowerCase().replace(/ /g, "-");
        
        // partial match
        entry = DesktopEntries.applications.values.find(a => 
            a.id.toLowerCase().includes(name) || 
            name.includes(a.id.toLowerCase())
        );
        
        if (!entry) {
            console.log(`[Icons] No desktop entry found for WM class: "${name}"`);
        }
        
        return entry ?? null;
    }

    // TODO: fix class != title, for some apps, to have proper icon :)
    function getAppIcon(name: string, fallback: string): string {
        if (!name && !fallback) fallback = "application-default-icon";
        if (!name) {
            // console.log("ICONS.qml:: Calling with empty name string, fallback: " + fallback);
            name = fallback;
        }
    
        // First custom subs
        if (customIconEntrySubs.hasOwnProperty(name)) {
            const customFile = customIconEntrySubs[name];
            const customIconPath = "file:/" + Quickshell.env("HOME") + "/Flake/home/common/icons/" + customFile;
            // console.log(`Using custom mapped icon for "${name}": ${customIconPath}`);
            return customIconPath;
        }


        // Second desktoop entry subs
        let iconName = name;
        if (desktopEntrySubs.hasOwnProperty(name)) {
            iconName = desktopEntrySubs[name];
            // console.log(`Icon substitution: "${name}" -> "${iconName}"`);

            const themeIcon = Quickshell.iconPath(iconName.toLowerCase(), "");
            if (themeIcon) {
                // console.log(`Icon from theme: "${iconName}" -> "${themeIcon}"`);
                return themeIcon;
            }
        }

        // default .desktop entry
        const desktopIcon = getDesktopEntry(name)?.icon;
        if (desktopIcon) {
            // names or paths
            if (desktopIcon.startsWith("/") || desktopIcon.startsWith("file://")) {
                return desktopIcon.startsWith("file://") ? desktopIcon : "file://" + desktopIcon;
            }
            // iconPath
            const resolved = Quickshell.iconPath(desktopIcon, "");
            // console.log(`Icon desktop: "${name}" -> "${desktopIcon}" = ${resolved}`);
            return resolved;
        }

        // last resort fallback
        // console.log(`Using custom default.svg for "${name}"`);
        return "file://" + Quickshell.env("HOME") + "/Flake/home/common/icons/default.svg";
    }

    function getAppCategoryIcon(name: string, fallback: string): string {
        const categories = getDesktopEntry(name)?.categories;

        if (categories)
            for (const [key, value] of Object.entries(categoryIcons))
                if (categories.includes(key))
                    return value;
        return fallback;
    }

    function getNetworkIcon(strength: int): string {
        if (strength >= 80)
            return "signal_wifi_4_bar";
        if (strength >= 60)
            return "network_wifi_3_bar";
        if (strength >= 40)
            return "network_wifi_2_bar";
        if (strength >= 20)
            return "network_wifi_1_bar";
        return "signal_wifi_0_bar";
    }

    function getBluetoothIcon(icon: string): string {
        if (icon.includes("headset") || icon.includes("headphones"))
            return "headphones";
        if (icon.includes("audio"))
            return "speaker";
        if (icon.includes("phone"))
            return "smartphone";
        return "bluetooth";
    }

    function getWeatherIcon(code: string): string {
        if (weatherIcons.hasOwnProperty(code))
            return weatherIcons[code];
        return "air";
    }

    FileView {
        path: "/etc/os-release"
        onLoaded: {
            const lines = text().split("\n");
            let osId = lines.find(l => l.startsWith("ID="))?.split("=")[1];
            if (root.osIcons.hasOwnProperty(osId))
                root.osIcon = root.osIcons[osId];
            else {
                const osIdLike = lines.find(l => l.startsWith("ID_LIKE="))?.split("=")[1];
                if (osIdLike)
                    for (const id of osIdLike.split(" "))
                        if (root.osIcons.hasOwnProperty(id))
                            return root.osIcon = root.osIcons[id];
            }

            let nameLine = lines.find(l => l.startsWith("PRETTY_NAME="));
            if (!nameLine)
                nameLine = lines.find(l => l.startsWith("NAME="));
            root.osName = nameLine.split("=")[1].slice(1, -1);
        }
    }
}
