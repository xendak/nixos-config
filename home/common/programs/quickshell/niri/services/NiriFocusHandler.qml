import QtQuick
import "root:/" // Import your Niri singleton

QtObject {
    id: root
    
    property bool active: false
    property bool launcherHasFocus: false
    property bool internalUpdate: false // Flag to prevent binding loops
    signal cleared()
    
    // Monitor changes to the active client
    property Connections niriConnection: Connections {
        target: Niri
        function onActiveClientChanged() {
            if (root.active && !root.internalUpdate) {
                handleFocusChange();
            }
        }
    }
    
    function handleFocusChange() {
        console.log("Focus changed. Active client:", Niri.activeClient ? Niri.activeClient.appId : "null");
        
        if (Niri.activeClient && Niri.activeClient.appId) {
            // A real window has focus
            console.log("Real window focused:", Niri.activeClient.appId, "Title:", Niri.activeClient.title);
            
            // Only close if it's definitely NOT one of our windows
            // Be more permissive to avoid closing our own launcher
            const isExternalWindow = Niri.activeClient.appId !== "quickshell" && 
                                   !Niri.activeClient.appId.startsWith("caelestia-") &&
                                   !Niri.activeClient.title.includes("caelestia") &&
                                   Niri.activeClient.appId !== "";
            
            if (isExternalWindow) {
                console.log("External window focused, clearing launcher");
                root.launcherHasFocus = false;
                closeDelay.start();
            } else {
                console.log("Our window or unidentified window focused, keeping launcher");
                root.launcherHasFocus = true;
                closeDelay.stop();
            }
        } else {
            // No window has focus (null) - likely our layer shell launcher
            console.log("No window focused - likely layer shell surface");
            root.launcherHasFocus = true;
            closeDelay.stop();
        }
    }
    
    // Delayed close to prevent binding loops
    property Timer closeDelay: Timer {
        interval: 250 // Longer delay to avoid premature closing
        onTriggered: {
            if (root.active && !root.launcherHasFocus) {
                console.log("Delayed close triggered");
                root.internalUpdate = true; // Prevent binding loop
                root.cleared();
                Qt.callLater(() => root.internalUpdate = false);
            }
        }
    }
    
    onActiveChanged: {
        if (active) {
            console.log("Focus checker activated");
            root.launcherHasFocus = false;
            // Check current focus state
            handleFocusChange();
        } else {
            console.log("Focus checker deactivated");
            root.launcherHasFocus = false;
            closeDelay.stop();
        }
    }
}
