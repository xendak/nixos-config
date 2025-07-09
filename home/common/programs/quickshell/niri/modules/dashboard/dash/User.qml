import "root:/widgets"
import "root:/services"
import "root:/config"
import "root:/utils"
import Quickshell
import Quickshell.Io
import QtQuick

Row {
    id: root

    padding: Appearance.padding.large
    spacing: Appearance.spacing.large

    
     function formatUptime(totalSeconds) {
        if (isNaN(totalSeconds) || totalSeconds <= 0) {
            return "just now";
        }

        totalSeconds = Math.floor(totalSeconds);

        const days = Math.floor(totalSeconds / 86400);
        totalSeconds %= 86400;
        const hours = Math.floor(totalSeconds / 3600);
        totalSeconds %= 3600;
        const minutes = Math.floor(totalSeconds / 60);

        let parts = [];
        if (days > 0) parts.push(days + (days === 1 ? " day" : " days"));
        if (hours > 0) parts.push(hours + (hours === 1 ? " hour" : " hours"));
        if (minutes > 0) parts.push(minutes + (minutes === 1 ? " minute" : " minutes"));
        
        // If less than a minute, show this.
        if (parts.length === 0) {
            return "less than a minute";
        }

        return parts.join(", ");
    }

    StyledClippingRect {
        implicitWidth: info.implicitHeight
        implicitHeight: info.implicitHeight

        radius: Appearance.rounding.full
        color: Colours.palette.m3surfaceContainerHigh

        MaterialIcon {
            anchors.centerIn: parent

            text: "person"
            fill: 1
            font.pointSize: (info.implicitHeight / 2) || 1
        }

        CachingImage {
            anchors.fill: parent
            path: `$HOME/Flake/home/common/wallpapers/profile.png`
        }
    }

    Column {
        id: info

        spacing: Appearance.spacing.normal

        InfoLine {
            icon: Icons.osIcon
            text: Icons.osName
            colour: Colours.palette.m3primary
        }

        InfoLine {
            icon: "select_window_2"
            text: Quickshell.env("XDG_CURRENT_DESKTOP") || Quickshell.env("XDG_SESSION_DESKTOP")
            colour: Colours.palette.m3secondary
        }

        InfoLine {
            icon: "timer"
            text: uptimeProc.uptime
            colour: Colours.palette.m3tertiary

            Timer {
                running: true
                repeat: true
                interval: 15000
                onTriggered: uptimeProc.running = true
            }

            Process {
                id: uptimeProc

                property string uptime

                running: true
                // command: ["uptime", "-p"]
                command: ["awk", "{print $1}", "/proc/uptime"]
                stdout: SplitParser {
                    // onRead: data => uptimeProc.uptime = data
                    onRead: function(data) {
                        uptimeProc.uptime = root.formatUptime(Number(data))
                    }
                }
            }
        }
    }

    component InfoLine: Item {
        id: line

        required property string icon
        required property string text
        required property color colour

        implicitWidth: icon.implicitWidth + text.width + text.anchors.leftMargin
        implicitHeight: Math.max(icon.implicitHeight, text.implicitHeight)

        MaterialIcon {
            id: icon

            anchors.left: parent.left
            anchors.leftMargin: (DashboardConfig.sizes.infoIconSize - implicitWidth) / 2

            text: line.icon
            color: line.colour
            font.pointSize: Appearance.font.size.normal
            font.variableAxes: ({
                    FILL: 1
                })
        }

        StyledText {
            id: text

            anchors.verticalCenter: icon.verticalCenter
            anchors.left: icon.right
            anchors.leftMargin: icon.anchors.leftMargin
            text: `:  ${line.text}`
            font.pointSize: Appearance.font.size.normal

            width: DashboardConfig.sizes.infoWidth
            elide: Text.ElideRight
        }
    }
}
