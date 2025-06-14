pragma ComponentBehavior: Bound

import "root:/services"
import "root:/config"
import Quickshell
import Quickshell.Services.SystemTray
import QtQuick

Item {
    id: root

    required property ShellScreen screen

    readonly property var activeTrayModel: {
        if (!currentName.startsWith("traymenu")) {
            return null; // Not a tray menu, so no model to check
        }
        // Find the popout in the content whose name matches the current one
        const activePopout = content.children.find(c => c.name === currentName);
        // Return its modelData if found
        return activePopout ? activePopout.modelData : null;
    }

    property string currentName
    property real currentCenter
    property bool hasCurrent

    anchors.centerIn: parent

    implicitWidth: hasCurrent ? (content.children.find(c => c.shouldBeActive)?.implicitWidth ?? 0) + Appearance.padding.large * 2 : 0
    implicitHeight: (content.children.find(c => c.shouldBeActive)?.implicitHeight ?? 0) + Appearance.padding.large * 2
    // implicitHeight: {
    //     // Same logic for height.
    //     const t = activeTrayModel
    //     console.log(t.id)
    //     if(t.id === "udiskie") return 0
    //     const activeChild = content.children.find(c => c.shouldBeActive);
    //     return hasCurrent && activeChild ? activeChild.implicitHeight + Appearance.padding.large * 2 : 0;
    // }


    Item {
        id: content
        anchors.fill: parent
        anchors.margins: Appearance.padding.large

        clip: true

        Popout {
            name: "activewindow"
            source: "ActiveWindow.qml"
        }

        Popout {
            name: "network"
            source: "Network.qml"
        }

        Popout {
            name: "bluetooth"
            source: "Bluetooth.qml"
        }

        Popout {
            name: "battery"
            source: "Battery.qml"
        }

        Repeater {
            model: ScriptModel {

                values: [...SystemTray.items.values]
                // values: [...SystemTray.items.values].filter(item => item.title !== "udiskie")
            }

            Popout {
                id: trayMenu

                required property SystemTrayItem modelData
                required property int index

                // QUICK DIRTY FIX TO REMOVE UDISKIE
                name: `traymenu${index}`
                sourceComponent: trayMenuComp
                
                Connections {
                    target: root
                    function onHasCurrentChanged(): void {
                        if (root.hasCurrent && trayMenu.shouldBeActive) {
                            trayMenu.sourceComponent = null;
                            trayMenu.sourceComponent = trayMenuComp;
                        }
                    }
                }
                Component {
                    id: trayMenuComp
                    TrayMenu {
                        y: 0
                        popouts: root
                        trayItem: trayMenu.modelData.menu
                    }
                }
            }
        }
    }

    Behavior on implicitWidth {
        Anim {
            easing.bezierCurve: Appearance.anim.curves.emphasized
        }
    }

    Behavior on implicitHeight {
        enabled: root.implicitWidth > 0

        Anim {
            easing.bezierCurve: Appearance.anim.curves.emphasized
        }
    }

    Behavior on currentCenter {
        enabled: root.implicitWidth > 0

        NumberAnimation {
            duration: Appearance.anim.durations.normal
            easing.type: Easing.BezierSpline
            easing.bezierCurve: Appearance.anim.curves.emphasized
        }
    }

    component Popout: Loader {
        id: popout

        required property string name
        property bool shouldBeActive: root.currentName === name

        anchors.verticalCenter: parent.verticalCenter
        anchors.right: parent.right

        opacity: 0
        scale: 0.8
        active: false
        asynchronous: true

        states: State {
            name: "active"
            when: popout.shouldBeActive

            PropertyChanges {
                popout.active: true
                popout.opacity: 1
                popout.scale: 1
            }
        }

        transitions: [
            Transition {
                from: "active"
                to: ""

                SequentialAnimation {
                    Anim {
                        properties: "opacity,scale"
                        duration: Appearance.anim.durations.small
                    }
                    PropertyAction {
                        target: popout
                        property: "active"
                    }
                }
            },
            Transition {
                from: ""
                to: "active"

                SequentialAnimation {
                    PropertyAction {
                        target: popout
                        property: "active"
                    }
                    Anim {
                        properties: "opacity,scale"
                    }
                }
            }
        ]
    }

    component Anim: NumberAnimation {
        duration: Appearance.anim.durations.normal
        easing.type: Easing.BezierSpline
        easing.bezierCurve: Appearance.anim.curves.standard
    }
}
