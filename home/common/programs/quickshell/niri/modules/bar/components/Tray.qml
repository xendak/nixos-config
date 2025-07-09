import "root:/config"
import Quickshell.Services.SystemTray
import QtQuick

Item {
    id: root

    clip: true
    visible: width > 0 && height > 0

    implicitWidth: layout.implicitWidth
    implicitHeight: layout.implicitHeight

    // This list contains the IDs of the tray items you want to display.
    property var allowedIds: [
        "Fcitx",
        "vesktop"
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

        // Your transition for new items.
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
                // This correctly hides items that aren't in the list
                visible: allowedIds.includes(modelData.id)
            }
        }


        // Repeater {
        //     id: items
        //     // Set the model directly to the system tray items.
        //     model: SystemTray.items

        //     // The delegate is the template for each item in the model.
        //     delegate: TrayItem {
        //         // Control the visibility of each item within the delegate.
        //         // 'modelData' is the item from the model for this specific instance.
        //         visible: allowedIds.includes(modelData.id)
        //     }
        // }
    }

    // Your animation behaviors remain the same.
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
