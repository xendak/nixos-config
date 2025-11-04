// pragma ComponentBehavior: Bound
pragma Singleton

import "root:/utils"
import Quickshell
import Quickshell.Io
import QtQuick

Singleton {
    id: root

    // Properties matching Hyprland interface
    property int activeWsId: 1
    property var workspaces: ({})
    property var firstRun: true
    property var testing: workspaces
    property var lastClients: []
    property var clients: []
    
    // Internal state
    property bool isDestroying: false
    property var _rawWorkspaces: []
    property var _rawClients: []
    property bool _initialized: false
    property bool _workspacesFetched: false
    property bool _clientsFetched: false

    // Event stream process for real-time updates
    Process {
        id: eventProcess
        command: ["niri", "msg", "--json", "event-stream"]
        running: true
        
        stdout: SplitParser {
            onRead: data => {
                const lines = data.split('\n');
                for (const line of lines) {
                    if (line.trim()) {
                        parseEvent(line.trim());
                    }
                }
            }
        }
        
        onExited: function(exitCode, exitStatus) {
            if (exitCode !== 0 && !root.isDestroying) {
                console.log("Niri event stream exited with code:", exitCode);
                // Auto-restart after a delay
                Qt.callLater(() => {
                    if (!root.isDestroying) {
                        running = true;
                    }
                });
            }
        }
    }

    // Initial data fetch process
    Process {
        id: fetchProcess
        running: false

        stdout: SplitParser {
            onRead: data => {
                // console.log("Niri fetchProcess STDOUT received:", data ? data.substring(0, 200) + '...' : 'null');
                if (data) {
                    processFetchedData(data);
                }
            }
        }

        // stderr: SplitParser {
        //     onRead: data => {
        //         console.error("Niri fetchProcess STDERR:", data);
        //     }
        // }

        // onRunningChanged: {
        //     console.log("Niri fetchProcess running changed:", running);
        //     if (running) {
        //         console.log("Niri fetchProcess command:", command);
        //     }
        // }

        onExited: function(exitCode, exitStatus) {
            // console.log(`Niri fetchProcess exited with code: ${exitCode}, status: ${exitStatus}`);
            
            if (exitCode !== 0) {
                console.error("Niri fetchProcess failed with exit code:", exitCode);
            }
            running = false;
        }
    }

    // Client fetch process
    Process {
        id: clientFetchProcess
        running: false
        
        stdout: SplitParser {
            onRead: data => {
                // console.log("Niri clientFetchProcess STDOUT received:", data ? data.substring(0, 200) + '...' : 'null');
                if (data) {
                    processClientData(data);
                }
            }
        }
        
        // stderr: SplitParser {
        //     onRead: data => {
        //         console.error("Niri clientFetchProcess STDERR:", data);
        //     }
        // }
        
        onExited: function(exitCode, exitStatus) {
            // console.log(`Niri clientFetchProcess exited with code: ${exitCode}`);
            
            if (exitCode !== 0) {
                console.error("Niri clientFetchProcess failed with exit code:", exitCode);
            }
            running = false;
        }
    }

    // Workspace dispatch process
    Process {
        id: dispatchProcess
        running: false
        
        onExited: function(exitCode, exitStatus){
            running = false;
            if (exitCode !== 0) {
                console.log("Niri dispatch failed with code:", exitCode);
            }
        }
    }

    // Initialize on component creation
    Component.onCompleted: {
        // console.log("Niri.qml: Component.onCompleted called!");
        
        root.workspaces = {};
        root.clients = [];
        root._initialized = true;
        
        // console.log("Niri.qml: Starting initialization...");
        // console.log("Niri.qml: Testing direct command execution...");
        
        Qt.callLater(() => {
            // console.log("Niri.qml: Delayed initialization, fetching workspaces...");
            fetchWorkspaces();
        });
    }

    // Cleanup on destruction
    Component.onDestruction: {
        root.isDestroying = true;
        if (eventProcess.running) {
            eventProcess.running = false;
        }
    }

    // Fetch workspace data
    function fetchWorkspaces() {
        if (!fetchProcess.running) {
            // console.log("Niri.qml: Fetching workspaces...");
            fetchProcess.command = ["niri", "msg", "--json", "workspaces"];
            fetchProcess.running = true;
        }
    }

    // Fetch client data
    function fetchClients() {
        if (!clientFetchProcess.running) {
            // console.log("Niri.qml: Fetching clients...");
            clientFetchProcess.command = ["niri", "msg", "--json", "windows"];
            clientFetchProcess.running = true;
        }
    }

    function processFetchedData(data) {
        try {
            const parsedData = JSON.parse(data);
            
            if (Array.isArray(parsedData)) {
                processWorkspaces(parsedData);
                root._workspacesFetched = true;
                // console.log("Workspaces fetched successfully, now fetching clients...");
                fetchClients();
            } else {
                console.error("Niri fetchProcess parsed data is not an array:", JSON.stringify(parsedData));
            }
        } catch (e) {
            console.error("Niri fetchProcess JSON PARSE FAILED:", e);
            console.error("Raw data:", data);
        }
    }
    
    function processClientData(data) {
        // console.log("Niri.qml: Processing fetched client data...");
        
        try {
            const parsedData = JSON.parse(data);
            if (Array.isArray(parsedData)) {
                processClients(parsedData);
                root._clientsFetched = true;
                // console.log("Clients fetched and processed successfully");
            } else {
                console.error("Niri clientFetchProcess parsed data is not an array:", JSON.stringify(parsedData));
            }
        } catch (e) {
            console.error("Niri clientFetchProcess JSON PARSE FAILED:", e);
            console.error("Raw data:", data);
        }
    }

    // Process workspace data from JSON
    function processWorkspaces(data) {
        if (!Array.isArray(data)) {
            console.log("Invalid workspace data received");
            return;
        }
        
        // console.log("Niri.qml: Processing", data.length, "workspaces");
        if (root.workspaces == root.testing && !root.firstRun) return;
        const newWorkspaces = {};
        let newActiveWsId = root.activeWsId;
        
        for (const ws of data) {
            if (!ws || typeof ws.id === 'undefined') continue;
            
            const workspace = {
                id: ws.id || 0,
                idx: ws.idx || 0,
                name: ws.name || "",
                output: ws.output || "",
                isActive: ws.is_active || false,
                isFocused: ws.is_focused || false,
                isUrgent: ws.is_urgent || false,
                activeWindowId: ws.active_window_id || null,
                lastIpcObject: {
                    windows: 0
                }
            };
            
            newWorkspaces[ws.id] = workspace;
            
            if (ws.is_focused) {
                newActiveWsId = ws.id;
            }
        }
        
        root.workspaces = newWorkspaces;
        root.testing = workspaces;
        root.firstRun = false;
        root.activeWsId = newActiveWsId;
        root._rawWorkspaces = data;
        
        // console.log("Niri.qml: Finished processing workspaces. Active is", newActiveWsId);
        // console.log("Niri.qml: Workspaces created:", Object.keys(newWorkspaces));
    }

    function processClients(data) {
        // console.log("Niri.qml: Starting to process", data.length, "clients");
        
        if (!Array.isArray(data)) {
            console.log("Invalid client data received");
            return;
        }
        
        const newClients = [];

        // console.log("Niri.qml data[0]: ", JSON.stringify(data[0]));
        
        for (const client of data) {
            if (!client) continue;
            
            const workspaceObj = root.workspaces[client.workspace_id] || null;
            // console.log("Niri.qml: client:", JSON.stringify(client));
            
            // if (client.workspace_id && !workspaceObj) {
            //     console.warn("Client", client.id, "references workspace", client.workspace_id, "but workspace not found");
            // }
            const clientObj = {
                id: client.id || -1,
                address: (client.id || "").toString(),
                title: client.title,
                fakeTitle: client.title || qsTr("Desktop"),
                wmClass: client.class || client.app_id || "",
                app_id: client.app_id || "",
                layout: client.layout || null,
                pid: client.pid || 0,
                workspaceId: client.workspace_id || 0,
                is_focused: client.is_focused,
                floating: client.is_floating || false,
                urgent: client.urgent || false,
                workspace: workspaceObj,

                // mapped: client.mapped !== false,
                // monitor: 0,
                // fullscreen: client.is_fullscreen || false,
                // fakeFullscreen: false,
                // grouped: [],
                // swallowing: null,
                // focusHistoryId: 0,
                // x: client.x || 0,
                // y: client.y || 0,
                // width: client.width || 0,
                // height: client.height || 0
            };
            
            newClients.push(clientObj);
        }
        
        root.clients = newClients;
        root._rawClients = data;
        
        // console.log("Niri.qml: Processed", newClients.length, "clients");
        
        updateWorkspaceWindowCounts();
    }

    function updateWorkspaceWindowCounts() {
        const windowCounts = {};
        
        for (const client of root.clients) {
            if (client.workspaceId) {
                windowCounts[client.workspaceId] = (windowCounts[client.workspaceId] || 0) + 1;
            }
        }
        
        // console.log("Niri.qml: Calculated window counts:", JSON.stringify(windowCounts));
        
        const updatedWorkspaces = {};
        for (const [id, ws] of Object.entries(root.workspaces)) {
            const numericId = parseInt(id);
            const windowCount = windowCounts[numericId] || 0;
            
            const newWs = {};
            for (const [key, value] of Object.entries(ws)) {
                if (key === 'lastIpcObject') {
                    newWs[key] = {
                        windows: windowCount
                    };
                } else {
                    newWs[key] = value;
                }
            }
            updatedWorkspaces[id] = newWs;
        }
        
        root.workspaces = updatedWorkspaces;
        
        // console.log("Niri.qml: Updated workspace window counts");
        
        root.workspacesChanged();
    }

    function parseEvent(line) {
        try {
            const event = JSON.parse(line);

            // console.log("Niri.qml: Event received:", line);
            
            if (event.WorkspaceActivated) {
                const wsId = event.WorkspaceActivated.id;
                if (wsId !== root.activeWsId) {
                    // console.log("Niri.qml: Workspace activated:", wsId);
                    root.activeWsId = wsId;
                    fetchClients();
                }
            } 

            // || event.OverviewOpenedOrClosed
            else if (event.WindowOpenedOrChanged || event.WindowClosed) {            
                // TODO: REFETCH CLIENTS.
                fetchClients();
            }
            else if (event.WindowFocusChanged || event.WorkspaceActiveWindowChanged) {
                // TODO: SET FOCUSED CLIENT.
                fetchClients();
            }

        } catch (e) {
            console.log("Niri.qml: Could not parse event line as JSON:", line);
        }
    }

    // Dispatch function compatible with Hyprland interface
    function dispatch(command) {
        if (dispatchProcess.running) {
            console.log("Dispatch already running, ignoring command:", command);
            return;
        }
        
        const parts = command.split(' ');
        const action = parts[0];
        
        let niriCommand = ["niri", "msg", "action"];
        
        switch (action) {
            case "workspace":
                if (parts.length > 1) {
                    const wsId = parseInt(parts[1]);
                    if (!isNaN(wsId)) {
                        niriCommand.push("focus-workspace");
                        niriCommand.push(wsId.toString());
                    }
                }
                break;
            
            case "movetoworkspace":
                if (parts.length > 1) {
                    const wsId = parseInt(parts[1]);
                    if (!isNaN(wsId)) {
                        niriCommand.push("move-window-to-workspace");
                        niriCommand.push(wsId.toString());
                    }
                }
                break;
                
            case "togglefloating":
                niriCommand.push("toggle-window-floating");
                break;
                
            case "fullscreen":
                niriCommand.push("fullscreen-window");
                break;
                
            case "killactive":
                niriCommand.push("close-window");
                break;

            case "focus-window":
                if (parts.length > 1) {
                    const windowId = parts[1];
                    if (!isNaN(windowId)) {
                        niriCommand.push("focus-window");
                        niriCommand.push("--id");
                        niriCommand.push(windowId);
                    }
                }
                break;

            case "close-window":
                if (parts.length > 1) {
                    const windowId = parts[1];
                    if (!isNaN(windowId)) {
                        niriCommand.push("close-window");
                        niriCommand.push("--id");
                        niriCommand.push(windowId);
                    }
                }
                break;
                
            default:
                niriCommand.push(command);
                break;
        }
        
        dispatchProcess.command = niriCommand;
        dispatchProcess.running = true;
    }

    function getWorkspace(id) {
        return root.workspaces[id] || null;
    }

    function getClientsForWorkspace(wsId) {
        const clients = root.clients.filter(c => c.workspaceId === wsId);
        return clients;
    }

     function getClientsForActiveWorkspace() {
        const clients = root.clients.filter(c => c.workspaceId === root.activeWsId);
        return clients;
    }

    function focusWindow(windowId) {
        dispatch(`focus-window ${windowId}`)
    }
    function killWindow(windowId) {
        dispatch(`close-window ${windowId}`)
    }

    function debugState() {
        console.log("=== NIRI DEBUG STATE ===");
        console.log("Active workspace:", root.activeWsId);
        console.log("Workspaces:", JSON.stringify(root.workspaces, null, 2));
        console.log("Clients:", root.clients.length);
        console.log("Client workspace IDs:", root.clients.map(c => c.workspaceId));
        console.log("_workspacesFetched:", root._workspacesFetched);
        console.log("_clientsFetched:", root._clientsFetched);
        console.log("fetchProcess.running:", fetchProcess.running);
        console.log("clientFetchProcess.running:", clientFetchProcess.running);
        console.log("========================");
    }
    
    // Manual trigger for testing
    function manualRefresh() {
        console.log("Niri.qml: Manual refresh triggered");
        root._workspacesFetched = false;
        root._clientsFetched = false;
        fetchWorkspaces();
    }
}
