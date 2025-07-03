// LLM/ChatWrapper.qml (Final Version with FocusScope)

import "root:/config"
import Quickshell
import QtQuick

FocusScope { // The root item is now a FocusScope
    id: root

    required property PersistentProperties visibilities

    // --- Wrapper properties ---
    visible: root.visibilities.llmchat
    width: root.visibilities.llmchat ? 400 : 0
    height: parent.height

    clip: true
    anchors.right: parent.right
    anchors.top: parent.top
    anchors.bottom: parent.bottom

    // --- Focus management for the scope ---
    // When the wrapper becomes visible, the entire scope gets focus.
    focus: root.visible

    Behavior on width {
        NumberAnimation {
            duration: root.visibilities.llmchat ? 300 : 200
            easing.type: Easing.InOutQuad
        }
    }

    // --- Direct Instantiation of ChatContent ---
    ChatContent {
        id: chatContent
        anchors.fill: parent
        visibilities: root.visibilities
        
        // The content is visible when the wrapper is.
        visible: root.visible
    }

    // --- Connections to set focus on the input field ---
    Connections {
        target: root
        function onVisibleChanged() {
            if (root.visible) {
                // When the wrapper appears, tell the content to focus its input.
                chatContent.focusInput()
            }
        }
    }
}
