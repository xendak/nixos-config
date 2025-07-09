pragma Singleton

import Quickshell
import Quickshell.Io

Singleton {
    id: root

    property real bpm

    Process {
        running: false
        command: [`${Quickshell.shellRoot}/assets/beat_detector`,  `--no-log`]
        stdout: SplitParser {
            onRead: data => root.bpm = parseFloat(data)
        }
    }
}
