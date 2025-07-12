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

    implicitWidth: listWrapper.width + padding * 2
    implicitHeight: searchWrapper.height + listWrapper.height + padding * 2

    anchors.top: parent.top
    anchors.horizontalCenter: parent.horizontalCenter

    Item {
        id: listWrapper

        implicitWidth: list.width
        implicitHeight: list.height + root.padding

        anchors.horizontalCenter: parent.horizontalCenter
        anchors.bottom: searchWrapper.top
        anchors.bottomMargin: root.padding

        ContentList {
            id: list

            visibilities: root.visibilities
            search: search
            padding: root.padding
            rounding: root.rounding
        }
    }

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

            text: Rbw.isUnlocked ? "search" : "lock"
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

            placeholderText: Rbw.isUnlocked ? qsTr("Search passwords...") : qsTr("Enter master password")
            echoMode: Rbw.isUnlocked ? TextInput.Normal : TextInput.Password
            background: null

            Keys.onReturnPressed: function(event) {
                event.accepted = true;

                if (!Rbw.isUnlocked) {
                    Rbw.unlock(text);
                    text = "";
                    return;
                }

                const currentItem = list.currentList?.currentItem;
                if (!currentItem) {
                    return;
                }

                if (event.modifiers & Qt.ShiftModifier) {
                    Rbw.copyUsernameAndPassword(currentItem.modelData);
                } else {
                    Rbw.copyPassword(currentItem.modelData);
                }
                
                root.visibilities.rbw = false;
            }

            Keys.onUpPressed: {
                if (Rbw.isUnlocked && list.currentList) {
                    list.currentList.decrementCurrentIndex();
                }
            }

            Keys.onDownPressed: {
                if (Rbw.isUnlocked && list.currentList) {
                    list.currentList.incrementCurrentIndex();
                }
            }

            Keys.onEscapePressed: root.visibilities.rbw = false

            Connections {
                target: root.visibilities

                function onRbwChanged(): void {
                    if (root.visibilities.rbw) {
                        Rbw.refreshStatus();
                        search.forceActiveFocus();
                    } else {
                        search.text = "";
                        const current = list.currentList;
                        if (current)
                            current.currentIndex = 0;
                    }
                }
            }

            Connections {
                target: Rbw

                function onIsUnlockedChanged(): void {
                    if (Rbw.isUnlocked) {
                        search.text = "";
                        search.placeholderText = qsTr("Search passwords...");
                        search.echoMode = TextInput.Normal;
                    } else {
                        search.placeholderText = qsTr("Enter master password");
                        search.echoMode = TextInput.Password;
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
