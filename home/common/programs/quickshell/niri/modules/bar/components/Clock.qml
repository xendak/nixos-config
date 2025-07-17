import "root:/widgets"
import "root:/services"
import "root:/config"
import QtQuick

Column {
    id: root
    property color colour: Colours.palette.m3tertiary
    spacing: Appearance.spacing.small
    
    // MaterialIcon {
    //     text: "calendar_today"
    //     color: root.colour
    //     anchors.horizontalCenter: parent.horizontalCenter
    // }
    
    StyledText {
        anchors.horizontalCenter: parent.horizontalCenter
        horizontalAlignment: StyledText.AlignHCenter
        property var japaneseWeekdays: ["日", "月", "火", "水", "木", "金", "土"]
        // text: Time.format("dd")
        text: japaneseWeekdays[(new Date()).getDay()] + "\n" + Time.format("dd")
        font.pointSize: Appearance.font.size.smaller
        font.family: Appearance.font.family.mono
        color: root.colour
    }
    
    MaterialIcon {
        text: "schedule"
        color: root.colour
        anchors.horizontalCenter: parent.horizontalCenter
    }
    
    StyledText {
        anchors.horizontalCenter: parent.horizontalCenter
        horizontalAlignment: StyledText.AlignHCenter
        text: Time.format("HH\nmm")
        font.pointSize: Appearance.font.size.smaller
        font.family: Appearance.font.family.mono
        color: root.colour
    }
}
