import "root:/widgets"
import "root:/services"
import "root:/config"
import QtQuick

Column {
    id: root
    spacing: Appearance.spacing.normal

    StyledText {
        text: qsTr("Recording Settings")
        font.bold: true
        color: Colours.palette.m3onSurface
    }

    StyledText {
        text: qsTr("Mode:")
        color: Colours.palette.m3onSurfaceVariant
    }

    Row {
        spacing: Appearance.spacing.small

        StyledButton {
            text: qsTr("Window")
            highlighted: RecorderConfig.mode === "select"
            onClicked: RecorderConfig.mode = "select"
        }

        StyledButton {
            text: qsTr("Area")
            highlighted: RecorderConfig.mode === "area"
            onClicked: RecorderConfig.mode = "area"
        }

        StyledButton {
            text: qsTr("Fullscreen")
            highlighted: RecorderConfig.mode === "fullscreen"
            onClicked: RecorderConfig.mode = "fullscreen"
        }
    }

    StyledText {
        text: qsTr("Quality:")
        color: Colours.palette.m3onSurfaceVariant
    }

    Row {
        spacing: Appearance.spacing.small

        StyledButton {
            text: qsTr("Hardware")
            highlighted: RecorderConfig.quality === "hardware"
            onClicked: RecorderConfig.quality = "hardware"
        }

        StyledButton {
            text: qsTr("Lossless")
            highlighted: RecorderConfig.quality === "perfect"
            onClicked: RecorderConfig.quality = "perfect"
        }
    }

    Row {
        spacing: Appearance.spacing.small

        StyledText {
            text: qsTr("Timer (s):")
            color: Colours.palette.m3onSurfaceVariant
            anchors.verticalCenter: parent.verticalCenter
        }

        StyledTextField {
            id: timerInput
            width: 80
            text: RecorderConfig.timer.toString()
            onEditingFinished: {
                const val = parseInt(text);
                if (!isNaN(val) && val >= 0) {
                    RecorderConfig.timer = val;
                } else {
                    text = RecorderConfig.timer.toString();
                }
            }
        }
    }
}
