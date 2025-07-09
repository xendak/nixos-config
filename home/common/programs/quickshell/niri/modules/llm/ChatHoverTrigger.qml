import QtQuick
import "root:/config"

// Hover trigger for opening chat - separate from ChatWrapper
Rectangle {
    id: root
    
    required property PersistentProperties visibilities
    
    width: 10
    height: parent.height
    color: "transparent"
    
    anchors.right: parent.right
    anchors.top: parent.top
    anchors.bottom: parent.bottom
    
    // Only show when chat is closed
    visible: !root.visibilities.llmchat
    
    MouseArea {
        anchors.fill: parent
        hoverEnabled: true
        
        onEntered: {
            console.log("Hover trigger entered")
            if (!root.visibilities.llmchat) {
                hoverTimer.start()
            }
        }
        
        onExited: {
            console.log("Hover trigger exited")
            hoverTimer.stop()
        }
        
        Timer {
            id: hoverTimer
            interval: 500
            onTriggered: {
                console.log("Hover timer triggered, opening chat")
                if (parent.containsMouse && !root.visibilities.llmchat) {
                    root.visibilities.llmchat = true
                }
            }
        }
    }
    
    // Debug visualization (remove this later)
    Rectangle {
        anchors.fill: parent
        color: parent.parent.containsMouse ? "blue" : "green"
        opacity: 0.3
        
        Text {
            anchors.centerIn: parent
            text: "HOVER"
            rotation: 90
            color: "white"
            font.pixelSize: 8
        }
    }
}
