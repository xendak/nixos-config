import "root:/config"
import Quickshell.Services.SystemTray
import QtQuick

Item {
    id: root

    clip: true
    visible: width > 0 && height > 0

    implicitWidth: layout.implicitWidth
    implicitHeight: layout.implicitHeight

    property var blockedIds: [
        "udiskie",
        "nm-applet",
        "blueman"
    ]

    property var getVisibleItems: function() {
        var visibleItems = [];
        for (var i = 0; i < trayRepeater.count; i++) {
            var item = trayRepeater.itemAt(i);
            if (item && item.visible) {
                visibleItems.push(item);
            }
        }
        return visibleItems;
    }

    Column {
        id: layout
        spacing: Appearance.spacing.small

        add: Transition {
            NumberAnimation {
                properties: "scale"
                from: 0
                to: 1
                duration: Appearance.anim.durations.normal
                easing.type: Easing.BezierSpline
                easing.bezierCurve: Appearance.anim.curves.standardDecel
            }
        }

        Repeater {
            id: trayRepeater
            model: SystemTray.items

            delegate: TrayItem {
                // visible: allowedIds.includes(modelData.id)
                visible: !blockedIds.includes(modelData.id)
            }
        }
    }

    Behavior on implicitWidth {
        NumberAnimation {
            duration: Appearance.anim.durations.normal
            easing.type: Easing.BezierSpline
            easing.bezierCurve: Appearance.anim.curves.emphasized
        }
    }

    Behavior on implicitHeight {
        NumberAnimation {
            duration: Appearance.anim.durations.normal
            easing.type: Easing.BezierSpline
            easing.bezierCurve: Appearance.anim.curves.emphasized
        }
    }
}
