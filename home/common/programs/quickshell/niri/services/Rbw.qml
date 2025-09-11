pragma Singleton

import "root:/utils/scripts/fuzzysort.js" as Fuzzy
import Quickshell
import Quickshell.Io
import QtQuick

Singleton {
    id: root

    property bool isUnlocked: false
    property list<var> entries: []
    property list<var> preppedEntries: []
    readonly property string unlock_bin: Quickshell.env("HOME") + "/Flake/bin/unlock-rbw"


    Component.onCompleted: {
        checkUnlockStatus();
    }

    function checkUnlockStatus(): void {
        if (!unlockCheckProcess.running) {
            unlockCheckProcess.running = true;
        }
    }

    function unlock(password: string): void {
        if (!unlockProcess.running) {
            const escapedPassword = password.replace(/'/g, "'\\''");
            unlockProcess.command = [unlock_bin, escapedPassword];
            unlockProcess.running = true;
        }
    }


    function loadEntries(): void {
        if (!loadEntriesProcess.running) {
            loadEntriesProcess.running = true;
        }
    }

    function fuzzyQuery(search: string): var {
        if (!search.trim()) {
            return entries;
        }
        
        return Fuzzy.go(search, preppedEntries, {
            all: true,
            keys: ["name", "username", "folder"],
            scoreFn: r => {
                const nameScore = r[0].score;
                const usernameScore = r[1].score;
                const folderScore = r[2].score;
                
                // Prioritize name matches, then username, then folder
                if (nameScore > 0) return nameScore * 0.8 + usernameScore * 0.15 + folderScore * 0.05;
                if (usernameScore > 0) return usernameScore * 0.6 + folderScore * 0.1;
                return folderScore * 0.3;
            }
        }).map(r => r.obj.entry);
    }

    function copyPassword(entry: var): void {
        if (!getPasswordProcess.running) {
            getPasswordProcess.command = ["rbw", "get", entry.name];
            getPasswordProcess.running = true;
        }
    }

    function copyUsernameAndPassword(entry: var): void {
        if (!getUsernamePasswordProcess.running) {
            getUsernamePasswordProcess.username = entry.username;
            getUsernamePasswordProcess.command = ["rbw", "get", entry.name];
            getUsernamePasswordProcess.running = true;
        }
    }

    function copyToClipboard(text: string): void {
        if (!clipboardProcess.running) {
            clipboardProcess.command = ["wl-copy", text];
            clipboardProcess.running = true;
        }
    }
    function notifyCopied(option: bool): void {
        if (!notifyCopied.running) {
            const input = option ? "Password" : "Username and Password";
            const commandString = "Copied " + input + " to clipboard!";
            
            notifyProcess.command = ["notify-send", "-i", "bitwarden-tray.svg", "Bitwarden", commandString];
            notifyProcess.running = true;
        }
    }

    function refreshStatus(): void {
        checkUnlockStatus();
    }

    Process {
        id: notifyProcess
        running: false

        onExited: function(exitCOde, exitStatus) {
            running = false;
        }
    }

    Process {
        id: unlockCheckProcess
        command: ["rbw", "unlocked"]
        running: false
        
        onExited: function(exitCode, exitStatus) {
            const wasUnlocked = root.isUnlocked;
            root.isUnlocked = exitCode === 0;
            
            if (wasUnlocked !== root.isUnlocked) {
                if (root.isUnlocked) {
                    loadEntries();
                } else {
                    root.entries = [];
                    root.preppedEntries = [];
                }
            }
            running = false;
        }
    }

    Process {
        id: unlockProcess
        running: false
        
        onExited: function(exitCode, exitStatus) {
            checkUnlockStatus();
            running = false;
        }
    }
    
    function processEntriesJson(jsonData) {
        try {
            const allItems = JSON.parse(jsonData);
        
            const newEntries = allItems
                .filter(item => item.data) 
                .map(item => ({
                    name: item.name || "",
                    username: item.data.username || "",
                    folder: item.folder || "",
                    type: "login",
                    login: item.data 
                }));

            root.entries = newEntries;
        
            root.preppedEntries = newEntries.map(entry => ({
                name: Fuzzy.prepare(entry.name),
                username: Fuzzy.prepare(entry.username),
                folder: Fuzzy.prepare(entry.folder),
                entry: entry
            }));
        } catch (e) {
            console.log("Rbw.qml: Could not parse rbw JSON entries: ", e);
        }
    }

    function processEntries(lines) {
        try {
            const newEntries = [];
            for (const line of lines) {
                if (line.trim()) {
                    const parts = line.split('\t');
                    if (parts.length >= 1) {
                        newEntries.push({
                            name: parts[0] || "",
                            username: parts[1] || "",
                            folder: parts[2] || "",
                            type: "login"
                        });
                    }
                }
            }
            root.entries = newEntries;
            root.preppedEntries = newEntries.map(entry => ({
                name: Fuzzy.prepare(entry.name),
                username: Fuzzy.prepare(entry.username),
                folder: Fuzzy.prepare(entry.folder),
                entry: entry
            }));
        } catch (e) {
            console.log("Rbw.qml: Could not parse rbw entries: ", line);
        }
    }

    Process {
        id: loadEntriesProcess
        // command: ["rbw", "list", "--fields", "name,user,folder"]
        command: ["rbw", "list", "--raw"]
        running: false

        property string receivedData: ""

        stdout: SplitParser {
            onRead: data => {
                loadEntriesProcess.receivedData += data + "\n";
            }
        }
        
        onStarted: {
            receivedData = "";
        }

        onExited: function(exitCode, exitStatus) {
            // console.log(receivedData);
            if (exitCode === 0) {
                processEntriesJson(receivedData);
                // processEntries(receivedData.split('\n'));
            } else {
                console.log("loadEntriesProcess failed with exit code:", exitCode);
            }
            running = false;
        }
    }

    Process {
        id: getPasswordProcess
        running: false

        stdout: SplitParser {
            onRead: data => {
                const pw = data.trim();
                copyToClipboard(pw);
                notifyCopied(true);
            }
        }
        
        onExited: function(exitCode, exitStatus) {
            running = false;
        }
    }

    Process {
        id: getUsernamePasswordProcess
        running: false
        property string username: ""

        stdout: SplitParser {
            onRead: data => {
                const pw = data.trim();
                const user = getUsernamePasswordProcess.username.trim();
                copyToClipboard(user + "\n" + pw);
                notifyCopied(false);
            }
        }
        
        
        onExited: function(exitCode, exitStatus) {
            running = false;
        }
    }

    Process {
        id: clipboardProcess
        running: false
        
        onExited: function(exitCode, exitStatus) {
            running = false;
        }
    }
}
