pragma ComponentBehavior: Bound

import "root:/widgets"
import "root:/services"
import "root:/config"
import QtQuick
import QtQuick.Layouts

Item {
    id: root

    readonly property list<Workspace> workspaces: layout.children.filter(c => c.isWorkspace).sort((w1, w2) => w1.ws - w2.ws)
    readonly property var occupied: Niri.workspaces ? Object.values(Niri.workspaces).reduce((acc, curr) => {
        acc[curr.id] = curr.lastIpcObject.windows > 0;
        return acc;
    }, {}) : {}
    readonly property int groupOffset: Math.floor((Niri.activeWsId - 1) / BarConfig.workspaces.shown) * BarConfig.workspaces.shown

    implicitWidth: layout.implicitWidth
    implicitHeight: layout.implicitHeight

    // onOccupiedChanged: {
    //     console.log("Workspaces.qml - Occupied lookup table updated:", JSON.stringify(occupied));
    // }

    // Timer {
    //     interval: 5000
    //     running: true
    //     repeat: true
    //     onTriggered: {
    //         console.log("=== WORKSPACES DEBUG ===");
    //         console.log("Niri.workspaces:", JSON.stringify(Niri.workspaces));
    //         console.log("Occupied:", JSON.stringify(occupied));
    //         console.log("Active WS:", Niri.activeWsId);
    //         Niri.debugState();
    //         if (!Niri._workspacesFetched) {
    //                     console.log("Trying manual refresh...");
    //                     Niri.manualRefresh();
    //                 }
    //     }
    // }

    ColumnLayout {
        id: layout

        spacing: 0
        layer.enabled: true
        layer.smooth: true

        Repeater {
            model: BarConfig.workspaces.shown

            Workspace {
                occupied: root.occupied
                groupOffset: root.groupOffset
            }
        }
    }

    Loader {
        active: BarConfig.workspaces.occupiedBg
        asynchronous: true

        z: -1
        anchors.fill: parent

        sourceComponent: OccupiedBg {
            workspaces: root.workspaces
            occupied: root.occupied
            groupOffset: root.groupOffset
        }
    }

    Loader {
        active: BarConfig.workspaces.activeIndicator
        asynchronous: true

        sourceComponent: ActiveIndicator {
            workspaces: root.workspaces
            mask: layout
            maskWidth: root.width
            maskHeight: root.height
            groupOffset: root.groupOffset
        }
    }


    MouseArea {
        anchors.fill: parent

        onPressed: event => {
            const ws = layout.childAt(event.x, event.y).index + root.groupOffset + 1;
            Niri.dispatch(`workspace ${ws}`);
        }
    }
}
