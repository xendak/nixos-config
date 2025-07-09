import "root:/widgets"
import "root:/services"
import Quickshell
import Quickshell.Io

Scope {
    id: root

    property bool launcherInterrupted

    IpcHandler {
        target: "shortcuts"
        
        function toggleLauncher(): void {
            root.launcherInterrupted = false;
            if (!root.launcherInterrupted) {
                const visibilities = Visibilities.getForActive();
                visibilities.launcher = !visibilities.launcher;
            }
            root.launcherInterrupted = false;
        }
        
        function toggleSession(): void {
            const visibilities = Visibilities.getForActive();
            visibilities.session = !visibilities.session;
        }
        
        function toggleLlmChat(): void {
            root.launcherInterrupted = false;
            if (!root.launcherInterrupted) {
                const visibilities = Visibilities.getForActive();
                visibilities.llmchat = !visibilities.llmchat;
            }
            root.launcherInterrupted = false;
        }
        
        function interruptLauncher(): void {
            root.launcherInterrupted = true;
        }

        function clearNotifs(): void {
            for (const notif of Notifs.list)
                notif.popup = false;
        }

        function brightnessUp(): void {
            Brightness.increaseBrightness();
        }
        
        function brightnessDown(): void {
            Brightness.decreaseBrightness();
        }
        
        function mediaToggle(): void {
            const active = Players.active;
            if (active && active.canTogglePlaying)
                active.togglePlaying();
        }
        
        function mediaPrev(): void {
            const active = Players.active;
            if (active && active.canGoPrevious)
                active.previous();
        }
        
        function mediaNext(): void {
            const active = Players.active;
            if (active && active.canGoNext)
                active.next();
        }
        
        function mediaStop(): void {
            Players.active?.stop();
        }
        
        function volumeUp(): void {
            Audio.increaseVolume();
        }
        
        function volumeDown(): void {
            Audio.decreaseVolume();
        }

        function volumeMute(): void {
            Audio.toggleMute();
        }
    }
}
