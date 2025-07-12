pragma Singleton

import Quickshell

Singleton {
    property var screens: ({})
    property var panels: ({})

    function getForActive(): PersistentProperties {
        // TODO: implement a niri msg to get output
        if (Object.keys(screens).length === 1) {
            return screens[Object.keys(screens)[0]];
        }

        return Object.entries(screens).find(s => s[0].slice(s[0].indexOf('"') + 1, s[0].lastIndexOf('"')) === Hyprland.focusedMonitor.name)[1];
    }
}

