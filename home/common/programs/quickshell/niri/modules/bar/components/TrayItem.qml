pragma ComponentBehavior: Bound

import "root:/widgets"
import "root:/config"
import Quickshell
import Quickshell.Widgets
import Quickshell.Services.SystemTray
import QtQuick

Item {
    id: root
    required property SystemTrayItem modelData
    
    width: modelData ? Appearance.font.size.small * 2 : 0
    height: modelData ? Appearance.font.size.small * 2 : 0
    
    Loader {
        id: loader
        anchors.fill: parent
        active: root.modelData
        
        sourceComponent: MouseArea {
            acceptedButtons: Qt.LeftButton | Qt.RightButton
            
            onClicked: event => {
                if (event.button === Qt.LeftButton) {
                    console.log("TrayItem: " + root.modelData.id);
                    root.modelData.activate();
                }
                else if (root.modelData.hasMenu)
                    menu.open();
            }
            
            QsMenuAnchor {
                id: menu
                menu: root.modelData.menu
                anchor.window: this.QsWindow.window
            }
            
            IconImage {
                id: icon
                source: {
                    let icon = root.modelData.icon;
                    console.log("TRAY.qml:: " + icon);
                    if (icon.includes("?path=")) {
                        const [name, path] = icon.split("?path=");
                        icon = `file://${path}/${name.slice(name.lastIndexOf("/") + 1)}`;
                    }
                    return icon;
                }
                asynchronous: true
                anchors.fill: parent
            }
        }
    }
}
