import "root:/widgets"
import "root:/services"
import "root:/config"
import "root:/modules/bar/popouts" as BarPopouts
import Quickshell
import QtQuick

StyledRect {
    id: root

    required property BarPopouts.Wrapper popouts
    property bool pinned: false

    radius: Appearance.rounding.full
    color: Colours.palette.m3surfaceContainer

    implicitWidth: icon.implicitWidth + Appearance.padding.small * 2
    implicitHeight: icon.implicitHeight + Appearance.padding.small * 2

    MaterialIcon {
        id: icon
        anchors.centerIn: parent
        text: Recorder.isRecording ? "radio_button_checked" : "videocam"
        color: Recorder.isRecording ? Colours.palette.m3error : Colours.palette.m3onSurfaceVariant
        font.pointSize: Appearance.font.size.normal
    }

    StyledRect {
        id: hoverLayer
        anchors.fill: parent
        radius: root.radius
        color: Colours.palette.m3onSurface
        opacity: mouseArea.pressed ? 0.1 : (mouseArea.containsMouse ? 0.08 : 0)

        Behavior on opacity {
            NumberAnimation {
                duration: Appearance.anim.durations.small
                easing.type: Easing.BezierSpline
                easing.bezierCurve: Appearance.anim.curves.standard
            }
        }
    }

    MouseArea {
        id: mouseArea
        anchors.fill: parent
        acceptedButtons: Qt.LeftButton | Qt.RightButton
        hoverEnabled: true
        cursorShape: Qt.PointingHandCursor

        onClicked: event => {
            if (event.button === Qt.LeftButton) {
                Recorder.toggle();
                root.pinned = false;
                root.popouts.hasCurrent = false;
            } else if (event.button === Qt.RightButton) {
                root.pinned = !root.pinned;
                if (root.pinned) {
                    root.popouts.currentName = "recorder";
                    root.popouts.currentCenter = Qt.binding(() => root.y + root.implicitHeight / 2);
                    root.popouts.hasCurrent = true;
                } else {
                    root.popouts.hasCurrent = false;
                }
            }
        }
    }
}
