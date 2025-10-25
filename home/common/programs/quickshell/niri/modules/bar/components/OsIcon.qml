import "root:/widgets"
import "root:/services"
import "root:/utils"
import "root:/config"

StyledText {
    text: Icons.osIcon
    font.pointSize: Appearance.font.size.smaller
    font.family: Appearance.font.family.mono
    color: Colours.palette.m3error

    StateLayer {
        anchors.fill: undefined
        anchors.centerIn: parent
        anchors.horizontalCenterOffset: 1

        implicitWidth: parent.implicitHeight + Appearance.padding.small * 2
        implicitHeight: implicitWidth

        radius: Appearance.rounding.full

        function onClicked(): void {
            const c = "hexecute";
            // console.log("NIX ICON:: executing");
            Cmd.cmdExecute(c);
        }
    }
}

