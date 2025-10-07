pragma Singleton

import Quickshell
import Quickshell.Io
import QtQuick

Singleton {
    id: root

    function execute(commandString: string): void {
        // console.log("CMD: ", commandString);
        executeProcess.command = ["sh", "-c", commandString];
        executeProcess.startDetached();
    }

    Process {
        id: executeProcess
    }
}
