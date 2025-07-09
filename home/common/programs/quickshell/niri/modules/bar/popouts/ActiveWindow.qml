import "root:/widgets"
import "root:/services" // Ensure Niri.qml is here
import "root:/utils"
import "root:/config"
import Quickshell.Widgets
import Quickshell.Wayland
import QtQuick

Item {
    id: root

    // Use Niri instead of Hyprland
    property var activeClient: {
        const activeWs = Niri.activeWsId;
        const activeWsObj = Niri.workspaces[activeWs];
        if (activeWsObj && activeWsObj.activeWindowId) {
            const client = Niri.clients.find(c => c.address === activeWsObj.activeWindowId.toString());
            console.log(JSON.stringify(client));
            return Niri.clients.find(c => c.address === activeWsObj.activeWindowId.toString());
        }
        return Niri.clients.find(c => c.workspaceId === activeWs);
    }

    implicitWidth: activeClient ? child.implicitWidth : -Appearance.padding.large * 2
    implicitHeight: child.implicitHeight

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
                // Use Niri instead of Hyprland
                source: Icons.getAppIcon(activeClient?.wmClass ?? "", "image-missing")
            }

            Column {
                id: details

                StyledText {
                    // Use Niri instead of Hyprland
                    text: activeClient?.title ?? ""
                    font.pointSize: Appearance.font.size.normal

                    elide: Text.ElideRight
                    width: preview.implicitWidth - icon.implicitWidth - detailsRow.spacing
                }

                StyledText {
                    // Use Niri instead of Hyprland
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

                captureSource: ToplevelManager.toplevels.values.find(t => t.title === activeClient?.title) ?? null
                
                live: visible

                constraintSize.width: BarConfig.sizes.windowPreviewSize
                constraintSize.height: BarConfig.sizes.windowPreviewSize
            }
        }
    }

    // Animation component remains the same
}
