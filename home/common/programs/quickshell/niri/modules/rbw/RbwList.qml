pragma ComponentBehavior: Bound

import "root:/widgets"
import "root:/services"
import "root:/config"
import Quickshell
import QtQuick
import QtQuick.Controls

ListView {
    id: root

    required property int padding
    required property TextField search
    required property PersistentProperties visibilities

    function getModelValues() {
        return Rbw.fuzzyQuery(search.text);
    }

    model: ScriptModel {
        values: root.getModelValues()
        onValuesChanged: root.currentIndex = 0
    }

    spacing: Appearance.spacing.small
    orientation: Qt.Vertical
    implicitHeight: (LauncherConfig.sizes.itemHeight + spacing) * Math.min(LauncherConfig.maxShown, count) - spacing

    highlightMoveDuration: Appearance.anim.durations.normal
    highlightResizeDuration: 0

    highlight: StyledRect {
        radius: Appearance.rounding.full
        color: Colours.palette.m3onSurface
        opacity: 0.08
    }

    delegate: RbwItem {
        visibilities: root.visibilities
    }

    ScrollBar.vertical: StyledScrollBar {}

    add: Transition {
        Anim {
            properties: "opacity,scale"
            from: 0
            to: 1
        }
    }

    remove: Transition {
        Anim {
            properties: "opacity,scale"
            from: 1
            to: 0
        }
    }

    move: Transition {
        Anim {
            property: "y"
        }
        Anim {
            properties: "opacity,scale"
            to: 1
        }
    }

    addDisplaced: Transition {
        Anim {
            property: "y"
            duration: Appearance.anim.durations.small
        }
        Anim {
            properties: "opacity,scale"
            to: 1
        }
    }

    displaced: Transition {
        Anim {
            property: "y"
        }
        Anim {
            properties: "opacity,scale"
            to: 1
        }
    }

    component Anim: NumberAnimation {
        duration: Appearance.anim.durations.normal
        easing.type: Easing.BezierSpline
        easing.bezierCurve: Appearance.anim.curves.standard
    }
}
