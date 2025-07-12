import "root:/widgets"
import "root:/services"
import "root:/utils"
import "root:/config"
import Quickshell.Widgets
import Quickshell.Wayland
import QtQuick

Item {
    id: root

    property var activeClient: Niri.clients.find(c => c.is_focused === true);

    implicitWidth: activeClient ? child.implicitWidth : -Appearance.padding.large * 2
    implicitHeight: child.implicitHeight

    function helper() {
        console.log(JSON.stringify(ToplevelManager.toplevels.values));
    }

    property var activeToplevel: activeClient ? ToplevelManager.toplevels.values.find(function(t) {
        return t.appId === activeClient.wmClass && t.title === activeClient.title;
    }) : null

    Component.onCompleted: {
        console.log("--- Dumping ToplevelManager Data ---");
        if (ToplevelManager && ToplevelManager.toplevels && ToplevelManager.toplevels.values) {
            console.log(`Found ${ToplevelManager.toplevels.values.length} toplevels.`);
            ToplevelManager.toplevels.values.forEach(function(toplevel, index) {
                try {
                    // Using JSON.stringify to see all available properties for each toplevel.
                    if (toplevel.activated) {
                        console.log(`[${index}]: ${JSON.stringify(toplevel)}`);
                        console.log("activeClient?: " + JSON.stringify(activeClient));
                        console.log("active?: " + JSON.stringify(activeToplevel));
                    }
                } catch (e) {
                    // Fallback if stringify fails (e.g., due to circular references).
                    console.log(`[${index}]: Could not stringify. Available keys: ${Object.keys(toplevel)}`);
                }
            });
        } else {
            console.log("ToplevelManager or its properties are not available at onCompleted.");
        }
        console.log("------------------------------------");
    }

    Column {
        id: child

        anchors.centerIn: parent
        spacing: Appearance.spacing.normal

        Row {
            id: detailsRow

            spacing: Appearance.spacing.normal

            IconImage {
                id: icon

                implicitSize: details.implicitHeight
                source: Icons.getAppIcon(activeClient?.wmClass ?? "", "image-missing")
            }

            Column {
                id: details

                StyledText {
                    text: activeClient?.title ?? ""
                    font.pointSize: Appearance.font.size.normal

                    elide: Text.ElideRight
                    width: preview.implicitWidth - icon.implicitWidth - detailsRow.spacing
                }

                StyledText {
                    text: activeClient?.wmClass ?? ""
                    color: Colours.palette.m3onSurfaceVariant

                    elide: Text.ElideRight
                    width: preview.implicitWidth - icon.implicitWidth - detailsRow.spacing
                }
            }
        }

        ClippingWrapperRectangle {
            color: "transparent"
            radius: Appearance.rounding.small

            ScreencopyView {
                id: preview

                captureSource: activeToplevel ?? null;
                live: visible

                constraintSize.width: BarConfig.sizes.windowPreviewSize
                constraintSize.height: BarConfig.sizes.windowPreviewSize
            }
        }
    }

    component Anim: NumberAnimation {
        duration: Appearance.anim.durations.normal
        easing.type: Easing.BezierSpline
        easing.bezierCurve: Appearance.anim.curves.emphasized
    }
}
