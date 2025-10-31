pragma ComponentBehavior: Bound

import "root:/widgets"
import "root:/services"
import "root:/utils"
import "root:/config"
import Quickshell
import Quickshell.Widgets
import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

Item {
    id: root
    
    required property Brightness.Monitor monitor
    
    implicitWidth: BarConfig.sizes.innerHeight
    implicitHeight: windowColumn.implicitHeight

    readonly property var sortedClients: clientModel.values
    
    function focusRelativeWindow(direction) {
        const clients = clientModel.values;
        if (clients.length === 0) return;
        
        const focusedIndex = clients.findIndex(c => c.is_focused);
        
        if (focusedIndex === -1) {
            const firstId = clients[0]?.id;
            if (firstId) Niri.focusWindow(firstId);
            return;
        }
        
        let nextIndex = focusedIndex + direction;
        if (nextIndex < 0) {
            nextIndex = clients.length - 1;
        } else if (nextIndex >= clients.length) {
            nextIndex = 0;
        }
        
        const nextClient = clients[nextIndex];
        const windowId = nextClient?.id;
        if (windowId) {
            Niri.focusWindow(windowId);
        }
    }
    
    Connections {
        target: Niri
        
        function onClientsChanged() {
            updateClientList();
        }
        
        function onActiveWsIdChanged() {
            updateClientList();
        }
    }
    
    Component.onCompleted: {
        updateClientList();
    }
    
    function updateClientList() {
        let clients = Niri.getClientsForActiveWorkspace();
        if (clients && clients.length > 0) {
        }
        
        // Sort by pos_in_scrolling_layout
        if (clients && clients.length > 0) {
            clients = clients.sort((a, b) => {
                const aPos = a.layout?.pos_in_scrolling_layout;
                const bPos = b.layout?.pos_in_scrolling_layout;
                
                // Floating windows (null pos_in_scrolling_layout) go first
                if (!aPos && !bPos) return 0;
                if (!aPos) return -1;
                if (!bPos) return 1;
                
                // Compare column (first element)
                if (aPos[0] !== bPos[0]) {
                    return aPos[0] - bPos[0];
                }
                
                // If same column, compare row (second element)
                return aPos[1] - bPos[1];
            });
            
            clients.forEach((c, i) => {
                const pos = c.layout?.pos_in_scrolling_layout;
            });
        }
        
        clientModel.values = clients || [];
    }
    
    ColumnLayout {
        id: windowColumn
        anchors.centerIn: parent
        spacing: BarConfig.workspaces.windowIconGap
        
        Repeater {
            id: repeater
            model: ScriptModel {
                id: clientModel
                values: []
            }
            
            delegate: Item {
                id: windowIconItem
                
                required property var modelData
                required property int index
                
                Layout.preferredWidth: BarConfig.sizes.innerHeight + Appearance.padding.normal
                Layout.preferredHeight: BarConfig.workspaces.windowIconSize + Appearance.padding.normal
                
                Component.onCompleted: {
                    if (modelData) {
                    }
                }
                
                // The actual icon/visual
                Loader {
                    id: iconLoader
                    anchors.centerIn: parent
                    
                    sourceComponent: BarConfig.workspaces.windowIconImage 
                                     ? imageIconComponent 
                                     : materialIconComponent
                }
                
                Component {
                    id: imageIconComponent
                    
                    Item {
                        implicitWidth: BarConfig.workspaces.windowIconSize + Appearance.padding.normal
                        implicitHeight: BarConfig.workspaces.windowIconSize + Appearance.padding.normal
                        
                        // Background for focused window
                        Rectangle {
                            anchors.centerIn: parent
                            width: parent.width
                            height: parent.height
                            // color: Colours.palette.m3primary
                            color: windowIconItem.modelData?.is_focused ? Colours.palette.m3primary : Colours.palette.m3outlineVariant
                            radius: Appearance.rounding.small
                            // opacity: windowIconItem.modelData?.is_focused ? 0.6 : 0.0
                            
                            Behavior on opacity {
                                NumberAnimation {
                                    duration: Appearance.anim.durations.normal
                                    easing.type: Easing.BezierSpline
                                    easing.bezierCurve: Appearance.anim.curves.standard
                                }
                            }
                        }
                        
                        IconImage {
                            id: img
                            anchors.centerIn: parent
                            implicitSize: BarConfig.workspaces.windowIconSize
                            source: Icons.getAppIcon(
                                windowIconItem.modelData?.wmClass,
                                "", 
                                "image-missing"
                            )
                        }
                    }
                }
                
                Component {
                    id: materialIconComponent
                    
                    Item {
                        implicitWidth: BarConfig.workspaces.windowIconSize + Appearance.padding.normal
                        implicitHeight: BarConfig.workspaces.windowIconSize + Appearance.padding.normal
                        
                        // Background for focused window
                        Rectangle {
                            anchors.centerIn: parent
                            width: parent.width
                            height: parent.height
                            // color: Colours.palette.m3primary
                            color: windowIconItem.modelData?.is_focused ? Colours.palette.m3primary : Colours.palette.m3outlineVariant
                            radius: Appearance.rounding.normal
                            // opacity: windowIconItem.modelData?.is_focused ? 0.8 : 0.0
                            
                            Behavior on opacity {
                                NumberAnimation {
                                    duration: Appearance.anim.durations.normal
                                    easing.type: Easing.BezierSpline
                                    easing.bezierCurve: Appearance.anim.curves.standard
                                }
                            }
                        }
                        
                        MaterialIcon {
                            anchors.centerIn: parent
                            font.pointSize: BarConfig.workspaces.windowIconSize
                            text: Icons.getAppCategoryIcon(
                                windowIconItem.modelData?.wmClass || 
                                "", 
                                "terminal"
                            )
                            color: windowIconItem.modelData?.is_focused ? Colours.palette.m3onPrimary : Colours.palette.m3primaryVariant
                            
                            Behavior on color {
                                NumberAnimation {
                                    duration: Appearance.anim.durations.normal
                                    easing.type: Easing.BezierSpline
                                    easing.bezierCurve: Appearance.anim.curves.standard
                                }
                            }
                        }
                    }
                }
                
                StateLayer {
                    anchors.fill: parent
                    radius: Appearance.rounding.small
                    
                    function onClicked() {
                        const windowId = windowIconItem.modelData.id;
                        
                        
                        if (windowId !== undefined && windowId !== null) {
                            Niri.focusWindow(windowId);
                        } else {
                            console.error("ERROR: Could not find window ID!");
                            console.error("Available keys:", Object.keys(windowIconItem.modelData));
                        }
                    }
                }
            }
        }
    }
}
