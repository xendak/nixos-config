import "root:/widgets"
import "root:/services"
import "root:/config"
import Quickshell
import QtQuick

Item {
    id: root

    required property var modelData
    required property PersistentProperties visibilities


    function getHostname(uriString) {
        // console.log("uriString: " + uriString);
        if (!uriString || !uriString.startsWith("http")) {
            return "";
        }
        try {
            // The URL object is a standard way to parse URLs
            return new URL(uriString).hostname;
        } catch (e) {
            console.log("Invalid URI for icon:", uriString);
            return "";
        }
    }

    // Determine the icon source URL
    property string iconUrl: {
        // We only care about the first URI for the icon
        console.log(root.modelData?.uris);
        const uri = root.modelData?.uris[0];
        // console.log("iconUrl: " + uri);
        if (uri) {
            const hostname = getHostname(uri);
            console.log("hostname: " + hostname)
            if (hostname) {
                // Use DuckDuckGo's icon service
                return "https://icons.duckduckgo.com/ip3/" + hostname + ".ico";
            }
        }
        return "";
    }

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

        // MaterialIcon {
        //     id: icon

        //     text: root.modelData?.type === "login" ? "account_circle" : 
        //           root.modelData?.type === "card" ? "credit_card" :
        //           root.modelData?.type === "identity" ? "badge" :
        //           root.modelData?.type === "note" ? "note" : "lock"
        //     font.pointSize: Appearance.font.size.extraLarge

        //     anchors.verticalCenter: parent.verticalCenter
        // }
        Item {
            id: icon
            width: Appearance.font.size.extraLarge
            height: Appearance.font.size.extraLarge
            anchors.verticalCenter: parent.verticalCenter

            // The website's icon
            Image {
                id: webIcon
                anchors.fill: parent
                source: root.iconUrl
                visible: source !== "" && status === Image.Ready
                fillMode: Image.PreserveAspectFit
            }

            // Fallback MaterialIcon
            MaterialIcon {
                id: fallbackIcon
                anchors.centerIn: parent
                visible: webIcon.status !== Image.Ready // Show if image is not ready/failed
                text: root.modelData?.type === "login" ? "account_circle" :
                      root.modelData?.type === "card" ? "credit_card" :
                      root.modelData?.type === "identity" ? "badge" :
                      root.modelData?.type === "note" ? "note" : "lock"
                font.pointSize: Appearance.font.size.extraLarge
            }
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
