pragma ComponentBehavior: Bound

import "root:/widgets"
import "root:/services"
import "root:/config"
import Quickshell
import QtQuick

Item {
    id: root

    required property PersistentProperties visibilities
    readonly property int padding: Appearance.padding.large
    readonly property int rounding: Appearance.rounding.large

    implicitWidth: 500
    implicitHeight: searchWrapper.height + padding * 2

    anchors.top: parent.top
    anchors.horizontalCenter: parent.horizontalCenter


    StyledRect {
        id: searchWrapper

        color: Colours.alpha(Colours.palette.m3surfaceContainer, true)
        radius: Appearance.rounding.full

        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        anchors.margins: root.padding

        implicitHeight: Math.max(searchIcon.implicitHeight, search.implicitHeight, clearIcon.implicitHeight)

        MaterialIcon {
            id: searchIcon

            anchors.verticalCenter: parent.verticalCenter
            anchors.left: parent.left
            anchors.leftMargin: root.padding

            text: "Terminal"
            color: Colours.palette.m3onSurfaceVariant
        }

        StyledTextField {
            id: search

            anchors.left: searchIcon.right
            anchors.right: clearIcon.left
            anchors.leftMargin: Appearance.spacing.small
            anchors.rightMargin: Appearance.spacing.small

            topPadding: Appearance.padding.larger
            bottomPadding: Appearance.padding.larger

            placeholderText: qsTr("Enter shell command ...")
            echoMode: TextInput.Normal
            background: null

            Keys.onReturnPressed: function(event) {
                event.accepted = true;

                root.visibilities.cmdlauncher = false;
                Cmd.execute(text);
                text = "";
                return;
            }


            Keys.onEscapePressed: root.visibilities.cmdlauncher = false

            Connections {
                target: root.visibilities

                function onCmdChanged(): void {
                    if (root.visibilities.cmdlauncher) {
                        search.forceActiveFocus();
                    } else {
                        search.text = "";
                    }
                }
            }

        }

        MaterialIcon {
            id: clearIcon

            anchors.verticalCenter: parent.verticalCenter
            anchors.right: parent.right
            anchors.rightMargin: root.padding

            width: search.text ? implicitWidth : implicitWidth / 2
            opacity: {
                if (!search.text)
                    return 0;
                if (mouse.pressed)
                    return 0.7;
                if (mouse.hovered)
                    return 0.8;
                return 1;
            }

            text: "close"
            color: Colours.palette.m3onSurfaceVariant

            MouseArea {
                id: mouse

                property bool hovered

                anchors.fill: parent
                hoverEnabled: true
                cursorShape: search.text ? Qt.PointingHandCursor : undefined

                onEntered: hovered = true
                onExited: hovered = false
                onClicked: {
                    if (search.text) {
                        search.text = "";
                        search.forceActiveFocus();
                    }
                }
            }

            Behavior on width {
                NumberAnimation {
                    duration: Appearance.anim.durations.small
                    easing.type: Easing.BezierSpline
                    easing.bezierCurve: Appearance.anim.curves.standard
                }
            }

            Behavior on opacity {
                NumberAnimation {
                    duration: Appearance.anim.durations.small
                    easing.type: Easing.BezierSpline
                    easing.bezierCurve: Appearance.anim.curves.standard
                }
            }
        }
    }
}

