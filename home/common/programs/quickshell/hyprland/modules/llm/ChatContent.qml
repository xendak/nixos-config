pragma ComponentBehavior: Bound

import "root:/widgets"
import "root:/services"
import "root:/config"
import Quickshell
import QtQuick
import QtQuick.Controls
import QtQuick.Shapes
import QtQuick.Layouts

Item {
    id: root

    required property PersistentProperties visibilities
    readonly property int padding: Appearance.padding.large

    // AI Provider configuration
    property var aiProviders: ({
        "ollama": {
            name: "Ollama",
            icon: "computer",
            color: "#4CAF50",
            baseUrl: "http://localhost:11434/api/generate",
            defaultModel: "llama3.1"
        },
        "gemini": {
            name: "Gemini",
            icon: "auto_awesome",
            color: "#4285F4",
            baseUrl: "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-pro:generateContent",
            defaultModel: "gemini-2.5-pro"
        },
        "claude": {
            name: "Claude",
            icon: "psychology",
            color: "#FF6B35",
            baseUrl: "https://api.anthropic.com/v1/messages",
            defaultModel: "claude-3-sonnet-20240229"
        }
    })

    property var providerList: ["ollama", "gemini", "claude"]

    property string currentProvider: "gemini"
    property string currentModel: aiProviders[currentProvider].defaultModel
    property bool isGenerating: false
    property bool ollamaConnected: false

    property string geminiApiKey: Quickshell.env("GEMINI_API_KEY") || ""
    property string claudeApiKey: Quickshell.env("CLAUDE_API_KEY") || ""

    enabled: true
    focus: true
    Keys.onPressed: (event) => {
        if (event.key === Qt.Key_Escape) {
            root.visibilities.llmchat = false;
            event.accepted = true;
        }
    }

    Column {
        anchors.fill: parent
        anchors.margins: root.padding
        spacing: root.padding

        RowLayout {
            width: parent.width

            MaterialIcon {
                text: aiProviders[currentProvider].icon
                color: aiProviders[currentProvider].color
                font.pointSize: Appearance.font.size.large
                Layout.alignment: Qt.AlignVCenter
            }

            StyledText {
                id: headerText
                text: qsTr("AI Assistant: %1").arg(aiProviders[currentProvider].name)
                font.pointSize: Appearance.font.size.large
                font.weight: 600
                color: Colours.palette.m3onSurface
                Layout.alignment: Qt.AlignVCenter
            }

            Item {
                Layout.fillWidth: true
            }

            MaterialIcon {
                id: clearButton
                text: "delete_sweep"
                color: clearMouseArea.containsMouse ? Colours.palette.m3primary : Colours.palette.m3onSurfaceVariant
                font.pointSize: Appearance.font.size.normal
                visible: chatModel.count > 0
                Layout.alignment: Qt.AlignVCenter

                MouseArea {
                    id: clearMouseArea
                    anchors.fill: parent
                    anchors.margins: -Appearance.padding.small
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onClicked: chatModel.clear()
                }
            }

            MaterialIcon {
                id: closeButton
                text: "close"
                color: closeMouseArea.containsMouse ? Colours.palette.m3error : Colours.palette.m3onSurfaceVariant
                font.pointSize: Appearance.font.size.normal
                Layout.alignment: Qt.AlignVCenter

                MouseArea {
                    id: closeMouseArea
                    anchors.fill: parent
                    anchors.margins: -Appearance.padding.small
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    enabled: root.visibilities.llmchat

                    onClicked: {
                        root.visibilities.llmchat = false
                    }
                }
            }
        }

        Rectangle {
            width: parent.width
            height: parent.height - headerText.height - inputArea.height - root.padding * 2
            color: Colours.alpha(Colours.palette.m3surfaceContainerLowest, true)
            radius: Appearance.rounding.small

            ScrollView {
                id: scrollView
                anchors.fill: parent
                anchors.margins: root.padding
                clip: true

                ListView {
                    id: chatList
                    model: chatModel
                    spacing: Appearance.spacing.normal
                    delegate: ChatMessage { width: chatList.width }
                    onContentHeightChanged: positionViewAtEnd()
                }
            }

            Column {
                visible: chatModel.count === 0
                anchors.centerIn: parent
                spacing: Appearance.spacing.normal

                MaterialIcon {
                    text: aiProviders[currentProvider].icon
                    color: aiProviders[currentProvider].color
                    font.pointSize: 48
                    anchors.horizontalCenter: parent.horizontalCenter
                }
                StyledText {
                    text: qsTr("Start a conversation with %1").arg(aiProviders[currentProvider].name)
                    color: Colours.palette.m3onSurfaceVariant
                    font.pointSize: Appearance.font.size.large
                    anchors.horizontalCenter: parent.horizontalCenter
                }
                StyledText {
                    text: currentProvider === "ollama" && !ollamaConnected ?
                          qsTr("Start Ollama service first: 'ollama serve'") :
                          qsTr("Type your message below to begin")
                    color: currentProvider === "ollama" && !ollamaConnected ?
                           Colours.palette.m3error :
                           Colours.palette.m3onSurfaceVariant
                    font.pointSize: Appearance.font.size.normal
                    anchors.horizontalCenter: parent.horizontalCenter
                }
            }

            Row {
                visible: isGenerating
                anchors.bottom: parent.bottom
                anchors.right: parent.right
                anchors.margins: root.padding
                spacing: 8

                MaterialIcon {
                    text: "hourglass_empty"
                    color: aiProviders[currentProvider].color
                    font.pointSize: 16
                    anchors.verticalCenter: parent.verticalCenter

                    RotationAnimation on rotation {
                        running: isGenerating
                        loops: Animation.Infinite
                        duration: 1000
                        from: 0
                        to: 360
                    }
                }

                StyledText {
                    text: qsTr("Generating...")
                    color: Colours.palette.m3onSurfaceVariant
                    font.pointSize: Appearance.font.size.small
                    anchors.verticalCenter: parent.verticalCenter
                }
            }
        }

        Rectangle {
            id: inputArea
            width: parent.width
            height: Math.min(150, Math.max(50, inputField.contentHeight + root.padding))
            color: Colours.alpha(Colours.palette.m3surfaceContainer, true)
            radius: Appearance.rounding.full
            opacity: isGenerating ? 0.6 : 1.0

            Behavior on height {
                NumberAnimation { duration: 100; easing.type: Easing.InOutQuad }
            }

            Behavior on opacity {
                NumberAnimation { duration: 150 }
            }

            Row {
                anchors.fill: parent
                anchors.leftMargin: root.padding
                anchors.rightMargin: root.padding / 2
                spacing: Appearance.spacing.small

                ScrollView {
                    width: parent.width - sendButton.width - parent.spacing
                    height: parent.height
                    clip: true

                    TextArea {
                        id: inputField
                        color: Colours.palette.m3onSurface
                        placeholderText: qsTr("Type a message (Tab to switch AI)")
                        wrapMode: TextArea.Wrap
                        selectByMouse: true
                        enabled: !isGenerating

                        background: Rectangle { color: "transparent" }

                        Keys.onTabPressed: (event) => {
                            var currentIndex = root.providerList.indexOf(root.currentProvider);
                            var nextIndex = 0;
                            const providerCount = root.providerList.length;

                            nextIndex = (currentIndex + 1) % providerCount;

                            root.currentProvider = root.providerList[nextIndex];
                            event.accepted = true;
                        }

                        Keys.onReturnPressed: {
                            if (event.modifiers & Qt.ShiftModifier) {
                                event.accepted = false;
                                return;
                            }
                            event.accepted = true;
                            sendMessage();
                        }

                        function sendMessage() {
                            var message = text.trim();
                            if (message.length > 0 && !isGenerating) {
                                chatModel.append({
                                    "message": message,
                                    "isUser": true,
                                    "provider": currentProvider,
                                    "timestamp": new Date()
                                });
                                text = "";
                                sendToAI(message);
                            }
                        }
                    }
                }

                Rectangle {
                    id: sendButton
                    width: 40
                    height: 40
                    radius: width / 2
                    color: inputField.text.trim().length > 0 && sendMouseArea.containsMouse && !isGenerating ?
                           aiProviders[currentProvider].color : "transparent"
                    anchors.verticalCenter: parent.verticalCenter

                    MaterialIcon {
                        text: isGenerating ? "stop" : "send"
                        color: inputField.text.trim().length > 0 && !isGenerating ?
                               aiProviders[currentProvider].color :
                               Colours.palette.m3onSurfaceVariant
                        font.pointSize: Appearance.font.size.normal
                        anchors.centerIn: parent
                    }

                    MouseArea {
                        id: sendMouseArea
                        anchors.fill: parent
                        enabled: (inputField.text.trim().length > 0 || isGenerating) && root.visibilities.llmchat
                        hoverEnabled: true
                        cursorShape: enabled ? Qt.PointingHandCursor : Qt.ArrowCursor

                        onClicked: {
                            if (isGenerating) {
                                isGenerating = false;
                            } else {
                                inputField.sendMessage()
                            }
                        }
                    }
                }
            }
        }
    }

    ListModel {
        id: chatModel
    }

    QtObject {
        id: networkManager

        function makeRequest(url, method, headers, data, callback) {
            var xhr = new XMLHttpRequest();
            xhr.onreadystatechange = function() {
                if (xhr.readyState === XMLHttpRequest.DONE) {
                    callback(xhr.status, xhr.responseText);
                }
            };
            xhr.open(method, url, true);

            for (var key in headers) {
                xhr.setRequestHeader(key, headers[key]);
            }

            if (data) {
                xhr.send(JSON.stringify(data));
            } else {
                xhr.send();
            }
        }
    }

    function sendToAI(message) {
        isGenerating = true;

        switch(currentProvider) {
            case "ollama":
                sendToOllama(message);
                break;
            case "gemini":
                sendToGemini(message);
                break;
            case "claude":
                sendToClaude(message);
                break;
        }
    }

    function sendToOllama(message) {
        var data = {
            "model": currentModel,
            "prompt": message,
            "stream": false
        };

        networkManager.makeRequest(
            aiProviders.ollama.baseUrl,
            "POST",
            {"Content-Type": "application/json"},
            data,
            function(status, response) {
                isGenerating = false;
                if (status === 200) {
                    try {
                        var result = JSON.parse(response);
                        chatModel.append({
                            "message": result.response,
                            "isUser": false,
                            "provider": "ollama",
                            "timestamp": new Date()
                        });
                    } catch (e) {
                        addErrorMessage("Failed to parse Ollama response: " + e.message);
                    }
                } else if (status === 0) {
                    addErrorMessage("Cannot connect to Ollama. Please start Ollama service:\n• Run 'ollama serve' in terminal\n• Or install Ollama from ollama.ai");
                } else {
                    addErrorMessage("Ollama request failed (Status: " + status + "). Check if Ollama is running and model '" + currentModel + "' is available.");
                }
            }
        );
    }

    function sendToGemini(message) {
        if (!geminiApiKey) {
            addErrorMessage("Gemini API key not configured. Set GEMINI_API_KEY environment variable.");
            isGenerating = false;
            return;
        }

        var data = {
            "contents": [{
                "parts": [{"text": message}]
            }]
        };

        var url = aiProviders.gemini.baseUrl + "?key=" + geminiApiKey;

        networkManager.makeRequest(
            url,
            "POST",
            {"Content-Type": "application/json"},
            data,
            function(status, response) {
                isGenerating = false;
                if (status === 200) {
                    try {
                        var result = JSON.parse(response);
                        if (result.candidates && result.candidates[0] && result.candidates[0].content) {
                            var aiResponse = result.candidates[0].content.parts[0].text;
                            chatModel.append({
                                "message": aiResponse,
                                "isUser": false,
                                "provider": "gemini",
                                "timestamp": new Date()
                            });
                        } else {
                            addErrorMessage("Invalid Gemini response format");
                        }
                    } catch (e) {
                        addErrorMessage("Failed to parse Gemini response: " + e.message);
                    }
                } else {
                    var errorMsg = "Gemini request failed (Status: " + status + ")";
                    try {
                        var errorResponse = JSON.parse(response);
                        if (errorResponse.error && errorResponse.error.message) {
                            errorMsg += ": " + errorResponse.error.message;
                        }
                    } catch (e) {
                        // Ignore parsing error for error response
                    }
                    addErrorMessage(errorMsg);
                }
            }
        );
    }

    function sendToClaude(message) {
        if (!claudeApiKey) {
            addErrorMessage("Claude API key not configured. Set CLAUDE_API_KEY environment variable.");
            isGenerating = false;
            return;
        }

        var data = {
            "model": currentModel,
            "max_tokens": 1024,
            "messages": [{
                "role": "user",
                "content": message
            }]
        };

        networkManager.makeRequest(
            aiProviders.claude.baseUrl,
            "POST",
            {
                "Content-Type": "application/json",
                "x-api-key": claudeApiKey,
                "anthropic-version": "2023-06-01"
            },
            data,
            function(status, response) {
                isGenerating = false;
                if (status === 200) {
                    try {
                        var result = JSON.parse(response);
                        var aiResponse = result.content[0].text;
                        chatModel.append({
                            "message": aiResponse,
                            "isUser": false,
                            "provider": "claude",
                            "timestamp": new Date()
                        });
                    } catch (e) {
                        addErrorMessage("Failed to parse Claude response: " + e.message);
                    }
                } else {
                    var errorMsg = "Claude request failed (Status: " + status + ")";
                    try {
                        var errorResponse = JSON.parse(response);
                        if (errorResponse.error && errorResponse.error.message) {
                            errorMsg += ": " + errorResponse.error.message;
                        }
                    } catch (e) {
                        // Ignore parsing error for error response
                    }
                    addErrorMessage(errorMsg);
                }
            }
        );
    }

    function addErrorMessage(errorText) {
        chatModel.append({
            "message": "Error: " + errorText,
            "isUser": false,
            "provider": "system",
            "timestamp": new Date(),
            "isError": true
        });
    }

    function focusInput() {
        Qt.callLater(() => {
            inputField.forceActiveFocus();
        });
    }

    function positionViewAtEnd() {
        if(scrollView.ScrollBar.vertical) {
            scrollView.ScrollBar.vertical.position = scrollView.ScrollBar.vertical.size;
        }
    }
}
