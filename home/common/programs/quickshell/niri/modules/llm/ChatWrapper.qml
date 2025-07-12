import "root:/config"
import Quickshell
import QtQuick

FocusScope {
    id: root

    required property PersistentProperties visibilities
    visible: root.visibilities.llmchat
    width: root.visibilities.llmchat ? 400 : 0
    height: parent.height

    clip: true
    anchors.right: parent.right
    anchors.top: parent.top
    anchors.bottom: parent.bottom

    focus: root.visible

    Behavior on width {
        NumberAnimation {
            duration: root.visibilities.llmchat ? 300 : 200
            easing.type: Easing.InOutQuad
        }
    }

    ChatContent {
        id: chatContent
        anchors.fill: parent
        visibilities: root.visibilities
        visible: root.visible
    }

    Connections {
        target: root
        function onVisibleChanged() {
            if (root.visible) {
                chatContent.focusInput()
            }
        }
    }
}
