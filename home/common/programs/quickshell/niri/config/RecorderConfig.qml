pragma Singleton

import Quickshell
import QtQuick

Singleton {
    readonly property string saveDir: `${Quickshell.env("HOME")}/Videos/Recordings`
    property string mode: "select"
    property string quality: "hardware"
    property int timer: 0
}
