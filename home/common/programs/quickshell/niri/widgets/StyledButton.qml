pragma ComponentBehavior: Bound

import "root:/services"
import "root:/config"
import QtQuick

Item {
    id: root

    property string text: ""
    property bool enabled: true
    property bool highlighted: false
    property color textColor: highlighted ? Colours.palette.m3onPrimary : Colours.palette.m3onSurface
    property color backgroundColor: highlighted ? Colours.palette.m3primary : Colours.palette.m3surface

    signal clicked()

    implicitWidth: Math.max(buttonText.implicitWidth + Appearance.padding.larger * 2, 64)
    implicitHeight: buttonText.implicitHeight + Appearance.padding.normal * 2

    opacity: enabled ? 1.0 : 0.5

    StyledRect {
        id: background

        anchors.fill: parent
        color: root.backgroundColor
        radius: Appearance.rounding.normal
        
        border.width: highlighted ? 0 : 1
        border.color: Colours.alpha(Colours.palette.m3outline, true)
    }

    StateLayer {
        id: stateLayer

        anchors.fill: parent
        radius: background.radius
        enabled: root.enabled

        function onClicked() {
            root.clicked();
        }
    }

    StyledText {
        id: buttonText

        anchors.centerIn: parent
        text: root.text
        color: root.textColor
        font.pointSize: Appearance.font.size.normal
        font.weight: Font.Medium
    }
}
