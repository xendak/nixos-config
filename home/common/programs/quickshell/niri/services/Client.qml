import QtQuick

// Client type for Niri compatibility
QtObject {
    id: root

    // Properties matching Hyprland.Client interface
    property string address: ""
    property string wmClass: ""
    property string title: ""
    property var workspace: null
    property int pid: 0
    property bool mapped: true
    property int monitor: 0
    property bool floating: false
    property bool fullscreen: false
    property bool fakeFullscreen: false
    property var grouped: []
    property var swallowing: null
    property int focusHistoryId: 0
    property int x: 0
    property int y: 0
    property int width: 0
    property int height: 0

    // Additional Niri-specific properties
    property int id: 0
    property int workspaceId: 0
    property bool isFullscreen: false
    property bool isFocused: false
    property string appId: ""
    property string class: ""
    
    // Compatibility aliases
    property alias class: root.wmClass
    property alias app_id: root.appId

    // Constructor function (for JavaScript object creation)
    function fromNiriData(data) {
        root.id = data.id || 0;
        root.address = data.id?.toString() || "";
        root.wmClass = data.class || data.app_id || "";
        root.appId = data.app_id || "";
        root.title = data.title || "";
        root.pid = data.pid || 0;
        root.workspaceId = data.workspace_id || 0;
        root.x = data.x || 0;
        root.y = data.y || 0;
        root.width = data.width || 0;
        root.height = data.height || 0;
        root.isFullscreen = data.is_fullscreen || false;
        root.isFocused = data.is_focused || false;
        root.floating = data.is_floating || false;
        root.fullscreen = data.is_fullscreen || false;
        root.mapped = data.mapped !== false;
        
        return root;
    }
}
