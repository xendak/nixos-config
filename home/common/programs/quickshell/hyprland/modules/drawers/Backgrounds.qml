import "root:/services"
import "root:/config"
import "root:/modules/osd" as Osd
import "root:/modules/notifications" as Notifications
import "root:/modules/session" as Session
import "root:/modules/launcher" as Launcher
import "root:/modules/dashboard" as Dashboard
import "root:/modules/llm" as LLM
import "root:/modules/bar/popouts" as BarPopouts
import QtQuick
import QtQuick.Shapes

Shape {
    id: root

    required property Panels panels
    required property Item bar

    anchors.fill: parent
    anchors.margins: BorderConfig.thickness
    anchors.leftMargin: bar.implicitWidth
    preferredRendererType: Shape.CurveRenderer
    opacity: Colours.transparency.enabled ? Colours.transparency.base : 1

    Osd.Background {
        wrapper: panels.osd

        startX: root.width - panels.session.width
        startY: (root.height - wrapper.height) / 2 - rounding
    }

    Notifications.Background {
        wrapper: panels.notifications

        startX: root.width
        startY: 0
    }

    LLM.Background {
        wrapper: panels.llmchat
        startX: root.width - panels.session.width
        startY: -25
    }

    Session.Background {
        wrapper: panels.session

        startX: root.width
        startY: (root.height - wrapper.height) / 2 - rounding
    }

    Launcher.Background {
        wrapper: panels.launcher

        startX: (root.width - wrapper.width) / 2 - rounding
        startY: root.height
    }

    Dashboard.Background {
        wrapper: panels.dashboard

        startX: (root.width - wrapper.width) / 2 - rounding
        startY: 0
    }

    BarPopouts.Background {
        wrapper: panels.popouts
        invertBottomRounding: wrapper.y + wrapper.height + 1 >= root.height

        startX: 0
        startY: wrapper.y - rounding
    }
}
