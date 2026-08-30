pragma Singleton

import "root:/config"
import "root:/utils"
import Quickshell
import Quickshell.Io
import QtQuick

Singleton {
    id: root
    property bool isRecording: false
    property string currentFile: ""

    Process {
        id: recordProc
        onExited: (exitCode, exitStatus) => {
            if (exitCode !== 0 && exitCode !== 130) { // 130 is SIGINT
                console.log("[Recorder] wf-recorder exited with code:", exitCode);
            }
            root.isRecording = false;
            root.currentFile = "";
        }
    }

    Process {
        id: pickProc
        stdout: SplitParser {
            onRead: data => {
                const geom = data.trim();
                if (geom) {
                    startWithGeometry(geom);
                }
            }
        }
        onExited: (exitCode, exitStatus) => {
            if (exitCode !== 0) {
                console.log("[Recorder] Picker exited with code:", exitCode);
            }
        }
    }

    Timer {
        id: stopTimer
        onTriggered: stop()
    }

    function toggle(): void {
        if (isRecording) {
            stop();
        } else {
            start();
        }
    }

    function getRecorderArgs(geom: string): list<string> {
        let args = ["wf-recorder"];

        // Apply quality settings
        if (RecorderConfig.quality === "perfect") {
            args.push("-c", "libx264", "-p", "pix_fmt=yuv444p", "-p", "crf=0");
        } else if (RecorderConfig.quality === "hardware") {
            // AMD Hardware Encoder (VAAPI)
            args.push("-c", "h264_vaapi");
        } else {
            args.push("-c", "libx264");
        }

        // Apply geometry if provided (area or window)
        if (geom && geom.length > 0) {
            // IMPORTANT: only trim leading/trailing whitespace here.
            // wf-recorder's -g flag needs the space between "X,Y" and
            // "WxH" preserved (format is "X,Y WxH"). Stripping all
            // whitespace merges them into an unparsable string.
            const wfGeom = geom.trim();
            args.push("-g", wfGeom);
        }

        args.push("-f", root.currentFile);
        return args;
    }
    function start(): void {
        if (isRecording)
            return;

        Paths.mkdir("file://" + RecorderConfig.saveDir);

        const ts = new Date();
        const pad = n => n.toString().padStart(2, '0');
        const dateStr = `${ts.getFullYear()}-${pad(ts.getMonth() + 1)}-${pad(ts.getDate())}-${pad(ts.getHours())}-${pad(ts.getMinutes())}-${pad(ts.getSeconds())}`;

        if (RecorderConfig.mode === "fullscreen") {
            root.currentFile = `${RecorderConfig.saveDir}/${dateStr}_fullscreen.mp4`;
            recordProc.command = getRecorderArgs("");
            recordProc.running = true;
            root.isRecording = true;
        } else if (RecorderConfig.mode === "area") {
            root.currentFile = `${RecorderConfig.saveDir}/${dateStr}_area.mp4`;
            // Redirect stdin to prevent slurp from hanging on the QProcess pipe.
            // Default slurp output format is already "%x,%y %wx%h", which is
            // exactly what wf-recorder's -g wants — no need for a custom -f.
            pickProc.command = ["sh", "-c", "slurp < /dev/null"];
            pickProc.running = true;
        } else if (RecorderConfig.mode === "select") {
            root.currentFile = `${RecorderConfig.saveDir}/${dateStr}_window.mp4`;
            // tile_pos_in_workspace_view is only populated for floating windows —
            // niri doesn't expose an absolute screen position for tiled windows
            // over IPC (their position depends on the workspace's live scroll
            // offset, which isn't reported anywhere). So: try pick-window first
            // for a one-click floating-window pick, and fall back to a manual
            // slurp drag-select if the window turns out to be tiled.
            const script = `
                GEOM=$(niri msg --json pick-window | jq -r '
                    .layout as $l
                    | if ($l.tile_pos_in_workspace_view != null) then
                        (($l.tile_pos_in_workspace_view[0] + $l.window_offset_in_tile[0]) | floor) as $x
                        | (($l.tile_pos_in_workspace_view[1] + $l.window_offset_in_tile[1]) | floor) as $y
                        | "\\($x),\\($y) \\($l.window_size[0])x\\($l.window_size[1])"
                      else empty end
                ')
                if [ -n "$GEOM" ]; then
                    echo "$GEOM"
                else
                    slurp < /dev/null
                fi
            `;

            pickProc.command = ["sh", "-c", script];
            pickProc.running = true;
        }

        if (RecorderConfig.timer > 0 && root.isRecording) {
            stopTimer.interval = RecorderConfig.timer * 1000;
            stopTimer.start();
        }
    }

    function startWithGeometry(geom: string): void {
        if (!geom)
            return;

        recordProc.command = getRecorderArgs(geom);
        recordProc.running = true;
        root.isRecording = true;

        if (RecorderConfig.timer > 0) {
            stopTimer.interval = RecorderConfig.timer * 1000;
            stopTimer.start();
        }
    }

    function stop(): void {
        stopTimer.stop();
        if (isRecording) {
            // Send SIGINT (-2) to wf-recorder for a clean video mux
            Quickshell.execDetached(["pkill", "-2", "wf-recorder"]);
        }
    }
}
