import "root:/services"
import "root:/config"
import QtQuick
import QtQuick.Shapes

ShapePath {
    id: root
    required property Item wrapper
    required property bool invertBottomRounding
    readonly property real rounding: BorderConfig.rounding
    readonly property bool flatten: wrapper.width < rounding * 2
    readonly property real roundingX: flatten ? wrapper.width / 2 : rounding
    property real ibr: invertBottomRounding ? -1 : 1
    
    strokeWidth: -1
    fillColor: BorderConfig.colour
    
    PathLine {
        relativeX: -(root.wrapper.width + root.rounding)
        relativeY: 0
    }
    PathArc {
        relativeX: root.rounding
        relativeY: root.rounding
        radiusX: root.rounding
        radiusY: root.rounding
    }
    PathLine {
        relativeX: 0
        relativeY: root.wrapper.height - root.rounding * 2
    }
    PathArc {
        relativeX: root.rounding * root.ibr
        relativeY: root.rounding
        radiusX: root.rounding
        radiusY: root.rounding
    }
    PathLine {
        relativeX: root.wrapper.width - root.roundingX - root.rounding * root.ibr
        relativeY: 0
    }
    PathArc {
        relativeX: root.roundingX
        relativeY: root.rounding
        radiusX: Math.min(root.rounding, root.wrapper.width)
        radiusY: root.rounding
    }

    Behavior on fillColor {
        ColorAnimation {
            duration: Appearance.anim.durations.normal
            easing.type: Easing.BezierSpline
            easing.bezierCurve: Appearance.anim.curves.standard
        }
    }

    Behavior on ibr {
        NumberAnimation {
            duration: Appearance.anim.durations.normal
            easing.type: Easing.BezierSpline
            easing.bezierCurve: Appearance.anim.curves.standard
        }
    }
}
