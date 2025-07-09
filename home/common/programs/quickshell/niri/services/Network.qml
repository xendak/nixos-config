pragma Singleton

import Quickshell
import Quickshell.Io
import QtQuick

Singleton {
    id: root

    property string networkStatus: "Checking..."

    readonly property list<AccessPoint> networks: []
    readonly property AccessPoint active: networks.find(n => n.active) ?? null

    reloadableId: "network"

    Process {
        running: true
        command: ["nmcli", "m"]
        stdout: SplitParser {
            onRead: checkEthernet.running = true
        }
    }

    Process {
        id: checkEthernet
        command: ["nmcli", "-t", "-f", "TYPE,STATE", "d"]
        stdout: StdioCollector {
            onStreamFinished: {
                const rNetworks = root.networks;

                const clearNetworks = () => {
                    while(rNetworks.length > 0) {
                        rNetworks.pop().destroy();
                    }
                }

                if (text.includes("ethernet:connected")) {
                    root.networkStatus = "Ethernet";
                    clearNetworks();

                    rNetworks.push(apComp.createObject(root, {
                        lastIpcObject: {
                            active: true,
                            strength: 100,
                            // Hehe
                            frequency: 9999,
                            ssid: "Ethernet",
                            bssid: "Ethernet"
                        }
                    }));
                } else {
                    checkWifi.running = true;
                }
            }
        }
    }

    Process {
        id: checkWifi
        command: ["nmcli", "r", "wifi"]
        stdout: StdioCollector {
            onStreamFinished: {
                if (text.trim() === "disabled") {
                    root.networkStatus = "Wifi Off";
                    // Ensure the list is empty if wifi is off
                    if (root.networks.length > 0) {
                         while(root.networks.length > 0) {
                            root.networks.pop().destroy();
                        }
                    }
                } else {
                    root.networkStatus = "Wifi On";
                    getNetworks.running = true;
                }
            }
        }
    }

    Process {
        id: getNetworks
        command: ["nmcli", "-g", "ACTIVE,SIGNAL,FREQ,SSID,BSSID", "d", "w"]
        environment: ({
            LANG: "C",
            LC_ALL: "C"
        })
        stdout: StdioCollector {
            onStreamFinished: {
                // Ignore Wi-Fi scan results if we are on Ethernet
                if (root.networkStatus === "Ethernet") return;

                const PLACEHOLDER = "STRINGWHICHHOPEFULLYWONTBEUSED";
                const rep = new RegExp("\\\\:", "g");
                const rep2 = new RegExp(PLACEHOLDER, "g");

                const newNetworks = text.trim().split("\n").filter(line => line.trim() !== "").map(n => {
                    const net = n.replace(rep, PLACEHOLDER).split(":");
                    return {
                        active: net[0] === "yes",
                        strength: parseInt(net[1]) || 0,
                        frequency: parseInt(net[2]) || 0,
                        ssid: net[3],
                        bssid: net[4]?.replace(rep2, ":") ?? ""
                    };
                });

                const rNetworks = root.networks;
                const destroyed = rNetworks.filter(rn => !newNetworks.find(n => n.bssid === rn.bssid));

                for (const network of destroyed) {
                    const index = rNetworks.indexOf(network);
                    if (index > -1) {
                        rNetworks.splice(index, 1)[0].destroy();
                    }
                }

                for (const network of newNetworks) {
                    const match = rNetworks.find(n => n.bssid === network.bssid);
                    if (match) {
                        match.lastIpcObject = network;
                    } else {
                        rNetworks.push(apComp.createObject(root, {
                            lastIpcObject: network
                        }));
                    }
                }
            }
        }
    }

    component AccessPoint: QtObject {
        required property var lastIpcObject
        readonly property string ssid: lastIpcObject.ssid
        readonly property string bssid: lastIpcObject.bssid
        readonly property int strength: lastIpcObject.strength
        readonly property int frequency: lastIpcObject.frequency
        readonly property bool active: lastIpcObject.active
    }

    Component {
        id: apComp

        AccessPoint {}
    }
}
