pragma ComponentBehavior: Bound

import "root:/widgets"
import "root:/services"
import "root:/config"
import QtQuick

Item {
    id: root

    required property var modelData
    readonly property bool isUser: modelData?.isUser ?? false
    readonly property string message: modelData?.message ?? ""
    readonly property date timestamp: modelData?.timestamp ?? new Date()

    implicitHeight: messageContainer.height + Appearance.spacing.small

    Column {
        id: messageContainer
        
        width: parent.width
        spacing: Appearance.spacing.small

        Row {
            width: parent.width
            layoutDirection: root.isUser ? Qt.RightToLeft : Qt.LeftToRight
            
            Item {
                width: 32
                height: 32
                
                StyledRect {
                    anchors.fill: parent
                    radius: width / 2
                    color: root.isUser ? 
                           Colours.palette.m3primary : 
                           Colours.palette.m3secondary
                }

                MaterialIcon {
                    anchors.centerIn: parent
                    text: root.isUser ? "person" : "smart_toy"
                    color: root.isUser ? 
                           Colours.palette.m3onPrimary : 
                           Colours.palette.m3onSecondary
                    font.pointSize: Appearance.font.size.normal
                }
            }
        }

        Row {
            width: parent.width
            layoutDirection: root.isUser ? Qt.RightToLeft : Qt.LeftToRight

            Item {
                width: Math.min(messageText.implicitWidth + Appearance.padding.large * 2, 
                               parent.width - Appearance.padding.normal)
                height: messageText.implicitHeight + Appearance.padding.normal * 2

                StyledRect {
                    id: messageBubble
                    anchors.fill: parent
                    radius: Appearance.rounding.large
                    color: root.isUser ? 
                           Colours.palette.m3primaryContainer : 
                           Colours.palette.m3surfaceContainerHigh
                    
                    border.width: 1
                    border.color: root.isUser ?
                                  Colours.palette.m3outline :
                                  Colours.palette.m3outlineVariant
                }

                StyledText {
                    id: messageText
                    
                    anchors.fill: parent
                    anchors.margins: Appearance.padding.normal
                    
                    text: root.message
                    color: root.isUser ? 
                           Colours.palette.m3onPrimaryContainer : 
                           Colours.palette.m3onSurface
                    font.pointSize: Appearance.font.size.normal
                    wrapMode: Text.Wrap
                    
                    verticalAlignment: Text.AlignTop
                }
            }
        }
    }

    StyledText {
        id: timestampText
        
        anchors.bottom: parent.bottom
        anchors.right: root.isUser ? parent.right : undefined
        anchors.left: root.isUser ? undefined : parent.left
        anchors.margins: Appearance.padding.small
        
        text: Qt.formatTime(root.timestamp, "hh:mm")
        color: Colours.palette.m3onSurfaceVariant
        font.pointSize: Appearance.font.size.small
        
        opacity: messageMouseArea.containsMouse ? 1 : 0
        
        Behavior on opacity {
            NumberAnimation {
                duration: Appearance.anim.durations.small
                easing.type: Easing.BezierSpline
                easing.bezierCurve: Appearance.anim.curves.standard
            }
        }
    }

    MouseArea {
        id: messageMouseArea
        anchors.fill: parent
        hoverEnabled: true
        onPressed: mouse.accepted = false
    }
}

// MAYBE BOXES>
// import QtQuick
// import QtQuick.Controls
// import QtQuick.Layouts
// import "root:/widgets"
// import "root:/services"
// import "root:/config"

// Rectangle {
//     id: messageContainer
    
//     required property string message
//     required property bool isUser
//     property string provider: "system"
//     property date timestamp: new Date()
//     property bool isError: false
    
//     readonly property var providerConfig: ({
//         "ollama": { icon: "computer", color: "#4CAF50" },
//         "gemini": { icon: "auto_awesome", color: "#4285F4" },
//         "claude": { icon: "psychology", color: "#FF6B35" },
//         "system": { icon: "error", color: "#F44336" }
//     })
    
//     width: parent.width
//     height: messageLayout.height + 16
//     color: isUser ? 
//            Colours.alpha(Colours.palette.m3primaryContainer, 0.15) : 
//            isError ? 
//            Colours.alpha(Colours.palette.m3errorContainer, 0.15) :
//            "transparent"
//     radius: Appearance.rounding.medium
    
//     RowLayout {
//         id: messageLayout
//         anchors.left: parent.left
//         anchors.right: parent.right
//         anchors.top: parent.top
//         anchors.margins: 8
//         spacing: 12
        
//         // Avatar/Icon
//         Rectangle {
//             Layout.preferredWidth: 36
//             Layout.preferredHeight: 36
//             Layout.alignment: Qt.AlignTop
//             radius: 18
//             color: isUser ? 
//                    Colours.palette.m3primary : 
//                    isError ? 
//                    Colours.palette.m3error :
//                    providerConfig[provider].color
            
//             MaterialIcon {
//                 text: isUser ? "person" : providerConfig[provider].icon
//                 color: "white"
//                 font.pointSize: 16
//                 anchors.centerIn: parent
//             }
//         }
        
//         // Message content
//         Column {
//             Layout.fillWidth: true
//             Layout.alignment: Qt.AlignTop
//             spacing: 4
            
//             // Header with sender and timestamp
//             Row {
//                 spacing: 8
                
//                 StyledText {
//                     text: isUser ? qsTr("You") : 
//                           isError ? qsTr("System") :
//                           providerConfig[provider].name || provider
//                     font.weight: Font.Bold
//                     color: isUser ? 
//                            Colours.palette.m3primary : 
//                            isError ?
//                            Colours.palette.m3error :
//                            providerConfig[provider].color
//                     font.pointSize: Appearance.font.size.small
//                 }
                
//                 StyledText {
//                     text: Qt.formatDateTime(timestamp, "hh:mm")
//                     color: Colours.palette.m3onSurfaceVariant
//                     font.pointSize: Appearance.font.size.small
//                 }
//             }
            
//             // Message text
//             StyledText {
//                 text: message
//                 color: isError ? 
//                        Colours.palette.m3error :
//                        Colours.palette.m3onSurface
//                 font.pointSize: Appearance.font.size.normal
//                 wrapMode: Text.Wrap
//                 width: messageLayout.width - 58 // Account for avatar and spacing
                
//                 // Make error messages italic
//                 font.italic: isError
                
//                 // Link handling
//                 onLinkActivated: (link) => {
//                     Qt.openUrlExternally(link)
//                 }
//             }
//         }
//     }
    
//     // Subtle border for user messages
//     Rectangle {
//         anchors.fill: parent
//         color: "transparent"
//         border.color: isUser ? 
//                      Colours.alpha(Colours.palette.m3primary, 0.2) : 
//                      "transparent"
//         border.width: isUser ? 1 : 0
//         radius: parent.radius
//         visible: isUser
//     }
// }
