pragma Singleton

import Quickshell
import Quickshell.Services.Pipewire

Singleton {
    id: root

    readonly property PwNode sink: Pipewire.defaultAudioSink
    readonly property PwNode source: Pipewire.defaultAudioSource

    readonly property bool muted: sink?.audio?.muted ?? false
    readonly property real volume: sink?.audio?.volume ?? 0

    function setVolume(volume: real): void {
        if (sink?.ready && sink?.audio) {
            sink.audio.muted = false;
            sink.audio.volume = volume;
        }
    }

    function increaseVolume(): void {
        if (sink?.ready && sink?.audio) {
            const newVolume = Math.min(1.0, sink.audio.volume + 0.05);
            sink.audio.muted = false;
            sink.audio.volume = newVolume;
        }
    }
    
    function decreaseVolume(): void {
        if (sink?.ready && sink?.audio) {
            const newVolume = Math.max(0.0, sink.audio.volume - 0.05);
            sink.audio.volume = newVolume;
        }
    }
    
    function toggleMute(): void {
        if (sink?.ready && sink?.audio) {
            sink.audio.muted = !sink.audio.muted;
        }
    }

    PwObjectTracker {
        objects: [Pipewire.defaultAudioSink, Pipewire.defaultAudioSource]
    }
}
