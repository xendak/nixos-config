pragma ComponentBehavior: Bound

import "root:/widgets"
import "root:/services"
import "root:/config"
import Quickshell
import QtQuick
import QtQuick.Controls

Item {
    id: root

    required property PersistentProperties visibilities
    required property TextField search
    required property int padding
    required property int rounding

    property var currentList: rbwList.item

    anchors.horizontalCenter: parent.horizontalCenter
    anchors.bottom: parent.bottom

    clip: true

    implicitWidth: LauncherConfig.sizes.itemWidth
    implicitHeight: Rbw.isUnlocked ? Math.max(empty.height, rbwList.height) : unlockPrompt.height

    Loader {
        id: rbwList

        active: Rbw.isUnlocked
        asynchronous: true

        anchors.left: parent.left
        anchors.right: parent.right

        sourceComponent: RbwList {
            padding: root.padding
            search: root.search
            visibilities: root.visibilities
        }
    }

    Item {
        id: unlockPrompt

        visible: !Rbw.isUnlocked
        opacity: !Rbw.isUnlocked ? 1 : 0
        scale: !Rbw.isUnlocked ? 1 : 0.5

        implicitWidth: icon.width + text.width + Appearance.spacing.small
        implicitHeight: icon.height

        anchors.horizontalCenter: parent.horizontalCenter
        anchors.verticalCenter: parent.verticalCenter

        MaterialIcon {
            id: icon

            text: "lock"
            color: Colours.palette.m3onSurfaceVariant
            font.pointSize: Appearance.font.size.extraLarge

            anchors.verticalCenter: parent.verticalCenter
        }

        StyledText {
            id: text

            anchors.left: icon.right
            anchors.leftMargin: Appearance.spacing.small
            anchors.verticalCenter: parent.verticalCenter

            text: qsTr("Vault locked")
            color: Colours.palette.m3onSurfaceVariant
            font.pointSize: Appearance.font.size.larger
            font.weight: 500
        }

        Behavior on opacity {
            NumberAnimation {
                duration: Appearance.anim.durations.normal
                easing.type: Easing.BezierSpline
                easing.bezierCurve: Appearance.anim.curves.standard
            }
        }

        Behavior on scale {
            NumberAnimation {
                duration: Appearance.anim.durations.normal
                easing.type: Easing.BezierSpline
                easing.bezierCurve: Appearance.anim.curves.standard
            }
        }
    }

    Item {
        id: empty

        visible: Rbw.isUnlocked
        opacity: (root.currentList?.count === 0 && Rbw.isUnlocked) ? 1 : 0
        scale: (root.currentList?.count === 0 && Rbw.isUnlocked) ? 1 : 0.5

        implicitWidth: emptyIcon.width + emptyText.width + Appearance.spacing.small
        implicitHeight: emptyIcon.height

        anchors.horizontalCenter: parent.horizontalCenter
        anchors.verticalCenter: parent.verticalCenter

        MaterialIcon {
            id: emptyIcon

            text: "manage_search"
            color: Colours.palette.m3onSurfaceVariant
            font.pointSize: Appearance.font.size.extraLarge

            anchors.verticalCenter: parent.verticalCenter
        }

        StyledText {
            id: emptyText

            anchors.left: emptyIcon.right
            anchors.leftMargin: Appearance.spacing.small
            anchors.verticalCenter: parent.verticalCenter

            text: qsTr("No results")
            color: Colours.palette.m3onSurfaceVariant
            font.pointSize: Appearance.font.size.larger
            font.weight: 500
        }

        Behavior on opacity {
            NumberAnimation {
                duration: Appearance.anim.durations.normal
                easing.type: Easing.BezierSpline
                easing.bezierCurve: Appearance.anim.curves.standard
            }
        }

        Behavior on scale {
            NumberAnimation {
                duration: Appearance.anim.durations.normal
                easing.type: Easing.BezierSpline
                easing.bezierCurve: Appearance.anim.curves.standard
            }
        }
    }

    Behavior on implicitHeight {
        NumberAnimation {
            duration: Appearance.anim.durations.large
            easing.type: Easing.BezierSpline
            easing.bezierCurve: Appearance.anim.curves.emphasizedDecel
        }
    }
}
