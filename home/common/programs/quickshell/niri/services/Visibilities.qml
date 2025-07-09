pragma Singleton

import Quickshell

Singleton {
    property var screens: ({})
    property var panels: ({})

    function getForActive(): PersistentProperties {
        console.log(screens);
        // niri workaround for now.
        if (Object.keys(screens).length === 1) {
            return screens[Object.keys(screens)[0]];
        }

        return Object.entries(screens).find(s => s[0].slice(s[0].indexOf('"') + 1, s[0].lastIndexOf('"')) === Hyprland.focusedMonitor.name)[1];
    }
}

// // Visibilities.qml - Modified for Niri
// pragma Singleton
// import Quickshell

// Singleton {
//     property var screens: ({})
//     property var panels: ({})
//     property string activeMonitor: ""
    
//     // For Niri, we need to track the active monitor differently
//     // Option 1: Use the first/primary monitor (simplest approach)
//     function getForActive(): PersistentProperties {
//         // If you have only one monitor, just return the first screen
//         if (Object.keys(screens).length === 1) {
//             return screens[Object.keys(screens)[0]];
//         }
        
//         // If you have multiple monitors, you can:
//         // 1. Use a specific monitor name
//         // 2. Use the first monitor
//         // 3. Track active monitor via external process
        
//         // Option 1: Use specific monitor (replace "YOUR_MONITOR_NAME" with actual name)
//         if (activeMonitor && screens[activeMonitor]) {
//             return screens[activeMonitor];
//         }
        
//         // Fallback: Use first available screen
//         const firstScreen = Object.keys(screens)[0];
//         return screens[firstScreen];
//     }
    
//     // Option 2: If you want to track active monitor via Niri IPC
//     function updateActiveMonitor() {
//         // This would require calling niri msg to get current state
//         // For now, we'll use a simpler approach
//         const screenKeys = Object.keys(screens);
//         if (screenKeys.length > 0) {
//             activeMonitor = screenKeys[0]; // Use first screen as active
//         }
//     }
    
//     // Option 3: Set active monitor manually (you can call this from your IPC handler)
//     function setActiveMonitor(monitorName: string) {
//         if (screens[monitorName]) {
//             activeMonitor = monitorName;
//         }
//     }
    
//     // Initialize on startup
//     Component.onCompleted: {
//         updateActiveMonitor();
//     }
// }
