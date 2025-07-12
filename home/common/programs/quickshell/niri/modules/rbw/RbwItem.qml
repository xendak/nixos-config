import "root:/widgets"
import "root:/services"
import "root:/config"
import Quickshell
import QtQuick

Item {
    id: root

    required property var modelData
    required property PersistentProperties visibilities

    implicitHeight: LauncherConfig.sizes.itemHeight

    anchors.left: parent?.left
    anchors.right: parent?.right

    StateLayer {
        radius: Appearance.rounding.full

        function onClicked(): void {
            const shiftPressed = (mouse.modifiers & Qt.ShiftModifier) !== 0;
            if (shiftPressed) {
                Rbw.copyUsernameAndPassword(root.modelData);
            } else {
                Rbw.copyPassword(root.modelData);
            }
            root.visibilities.rbw = false;
        }
    }

    Item {
        anchors.fill: parent
        anchors.leftMargin: Appearance.padding.larger
        anchors.rightMargin: Appearance.padding.larger
        anchors.margins: Appearance.padding.smaller

        MaterialIcon {
            id: icon

            text: root.modelData?.type === "login" ? "account_circle" : 
                  root.modelData?.type === "card" ? "credit_card" :
                  root.modelData?.type === "identity" ? "badge" :
                  root.modelData?.type === "note" ? "note" : "lock"
            font.pointSize: Appearance.font.size.extraLarge

            anchors.verticalCenter: parent.verticalCenter
        }

        Item {
            anchors.left: icon.right
            anchors.leftMargin: Appearance.spacing.larger
            anchors.verticalCenter: icon.verticalCenter

            implicitWidth: parent.width - icon.width
            implicitHeight: name.implicitHeight + desc.implicitHeight

            StyledText {
                id: name

                text: root.modelData?.name ?? ""
                font.pointSize: Appearance.font.size.normal
            }

            StyledText {
                id: desc

                text: {
                    if (root.modelData?.username) {
                        return root.modelData.username;
                    }
                    if (root.modelData?.uri) {
                        return root.modelData.uri;
                    }
                    return root.modelData?.folder ?? "";
                }
                font.pointSize: Appearance.font.size.small
                color: Colours.alpha(Colours.palette.m3outline, true)

                elide: Text.ElideRight
                width: root.width - icon.width - Appearance.rounding.normal * 2

                anchors.top: name.bottom
            }
        }

        // Visual indicator for keyboard shortcuts
        Item {
            anchors.right: parent.right
            anchors.verticalCenter: parent.verticalCenter
            width: shortcutText.width + Appearance.padding.small
            height: shortcutText.height + Appearance.padding.small / 2

            StyledText {
                id: shortcutText

                anchors.centerIn: parent
                text: "⏎"
                font.pointSize: Appearance.font.size.small
                color: Colours.alpha(Colours.palette.m3outline, true)
                opacity: 0.6
            }
        }
    }
}
