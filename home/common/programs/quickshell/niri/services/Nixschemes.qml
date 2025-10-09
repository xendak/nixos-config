pragma Singleton

import "root:/utils/scripts/fuzzysort.js" as Fuzzy
import "root:/utils"
import Quickshell
import Quickshell.Io
import QtQuick

Singleton {
    id: root

    readonly property string currentNamePath: `${Paths.state}/scheme/current.txt`.slice(7)
    readonly property string setNamePath: `${Paths.state}/scheme/preview.txt`.slice(7)
    readonly property string path: Quickshell.env("HOME") + "/Programming/probe/python/cache/"

    readonly property list<Nixscheme> list: nixschemes.instances
    property bool showPreview: false
    readonly property string current: showPreview ? previewPath : actualCurrent
    property string previewPath
    property string actualCurrent

    readonly property list<var> preppedSchemes: list.map(w => ({
            name: Fuzzy.prepare(w.name),
            path: Fuzzy.prepare(w.path),
            scheme: w
        }))

    function fuzzyQuery(search: string): var {
        return Fuzzy.go(search, preppedSchemes, {
            all: true,
            keys: ["name", "path"],
            scoreFn: r => r[0].score * 0.9 + r[1].score * 0.1
        }).map(r => r.obj.scheme);
    }

    function setNixfile(theme: string): void {
        setScheme.command = [
            "sh", "-c",
            `nix-theme-switcher ${theme} && cat ${setNamePath} && cat ${setNamePath} > ${currentNamePath}`
        ]
        notifyScheme.command = [
            "sh", "-c", 
            `notify-send "Theme Manager" --expire-time=2000 --app-name="Theme Manager" --icon=preferences-desktop-theme "Theme switched to ${theme}"`
        ]
        notifyScheme.startDetached();
        setScheme.startDetached();
    }

    function setNixscheme(path: string): void {
        actualCurrent = path;
        setScheme.path = path;
        const schemeName = path.slice(path.lastIndexOf("/") + 1, path.lastIndexOf("."));
        setScheme.command = [
            "sh", "-c",
            `nix-theme-switcher ${schemeName} && cat ${setNamePath} && cat ${setNamePath} > ${currentNamePath}`
        ]
        notifyScheme.command = [
            "sh", "-c", 
            `notify-send "Theme Manager" --expire-time=2000 --app-name="Theme Manager" --icon=preferences-desktop-theme "Theme switched to ${schemeName}"`
        ]
        notifyScheme.startDetached();
        setScheme.startDetached();
    }

    function preview(path: string): void {
        previewPath = path;
        showPreview = false;
        const schemeName = path.slice(path.lastIndexOf("/") + 1, path.lastIndexOf("."));
        // getPreviewColoursProc.command = ["/home/flakes/tmp/nix-theme-switcher-preview", schemeName]
        getPreviewColoursProc.command = [
            "sh", "-c", 
            // `/home/flakes/tmp/nix-theme-switcher-preview ${schemeName} && cat /home/flakes/.local/state/caelestia/scheme/preview.txt`
            // `/home/flakes/tmp/helper ${schemeName} && cat ${Paths.state}/scheme/preview.txt`
            `nix-theme-switcher ${schemeName} && cat ${setNamePath}`
        ]

        getPreviewColoursProc.running = true;
    }

    function stopPreview(): void {
        showPreview = false;
        Colours.endPreviewOnNextChange = true;
    }

    reloadableId: "nixschemes"

    FileView {
        path: root.currentNamePath
        watchChanges: true
        onFileChanged: reload()
        onLoaded: root.actualCurrent = text().trim()
    }

    Process {
        id: getPreviewColoursProc

        stdout: SplitParser {
            splitMarker: ""
            onRead: data => {
                Colours.load(data, true);
                Colours.showPreview = true;
            }
        }
    }

    Process {
        id: setScheme
        property string path
        stdout: SplitParser {
            splitMarker: ""
            onRead: data => {
                console.log(data);
                Colours.load(data, true);
                // Colours.showPreview = true;
            }
        }
    }

    Process {
        id: notifyScheme
        property string path
    }

    Process {
        id: findSchemesProc
        running: true
        command: ["fd", ".", root.path, "-t", "f", "-e", "jpg", "-e", "jpeg", "-e", "png", "-e", "svg"]

        stderr: SplitParser {
            splitMarker: ""
            onRead: data => {
                console.error("Stderr from fd command:", data);
            }
        }

        stdout: SplitParser {
            splitMarker: ""
            onRead: data => {
                console.log("Received raw data:", JSON.stringify(data));

                const trimmedData = data.trim();

                if (trimmedData) {
                    const fileList = trimmedData.split("\n");
                    console.log(`Found ${fileList.length} items. Populating model...`);
                    nixschemes.model = fileList;
                } else {
                    console.log("No image files found in path. Setting model to empty array.");
                    nixschemes.model = [];
                }

                console.log("Final list count:", root.list.length);
            }
        }
    }

    Variants {
        id: nixschemes

        Nixscheme {}
    }

    component Nixscheme: QtObject {
        required property string modelData
        readonly property string path: modelData
        readonly property string name: path.slice(path.lastIndexOf("/") + 1, path.lastIndexOf("."))
    }
}
