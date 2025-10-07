pragma Singleton

import Quickshell
import Quickshell.Io
import QtQuick

Singleton {
    id: root

    function cmdExecute(commandString: string): void {
        executeCmdProcess.cmdCommand = commandString;
        executeCmdProcess.startDetached();
    }

    Process {
        id: executeCmdProcess
        property string cmdCommand
        command: ["sh", "-c", `${cmdCommand}`]
    }
}
