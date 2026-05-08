use std::collections::BTreeMap;
use zellij_tile::prelude::*;

#[derive(Default)]
struct State {
    managed: BTreeMap<String, (u32, bool)>,
    pane_manifest: PaneManifest,
}

register_plugin!(State);

impl ZellijPlugin for State {
    fn load(&mut self, _config: BTreeMap<String, String>) {
        set_selectable(false);
        request_permission(&[
            PermissionType::OpenTerminalsOrPlugins,
            PermissionType::ReadApplicationState,
            PermissionType::ChangeApplicationState,
        ]);
        subscribe(&[EventType::PaneUpdate]);
    }

    fn update(&mut self, event: Event) -> bool {
        if let Event::PaneUpdate(manifest) = event {
            self.pane_manifest = manifest;
        }
        false
    }

    fn pipe(&mut self, pipe_message: PipeMessage) -> bool {
        if pipe_message.name == "toggle" {
            if let Some(kind) = &pipe_message.payload {
                self.handle_toggle(kind);
            }
        }
        false
    }

    fn render(&mut self, _rows: usize, _cols: usize) {}
}

impl State {
    fn get_editor_cwd(&self) -> String {
        self.pane_manifest
            .panes
            .values()
            .flat_map(|v| v.iter())
            .find(|p| p.title == "Editor")
            .and_then(|p| {
                let pane_id = if p.is_plugin {
                    PaneId::Plugin(p.id)
                } else {
                    PaneId::Terminal(p.id)
                };
                get_pane_cwd(pane_id).ok()
            })
            .map(|path| path.to_string_lossy().into_owned())
            .unwrap_or_else(|| ".".into())
    }

    fn handle_toggle(&mut self, kind: &str) {
        let destroy_on_toggle = kind == "horizontal";
        let floating: bool = kind == "float";

        if let Some(&(pane_id, is_hidden)) = self.managed.get(kind) {
            if self.pane_exists(pane_id) {
                let pid = PaneId::Terminal(pane_id);

                if destroy_on_toggle {
                    close_terminal_pane(pane_id);
                    self.managed.remove(kind);
                } else {
                    if is_hidden {
                        show_pane_with_id(pid, floating, true);
                        self.managed.insert(kind.to_string(), (pane_id, false));
                    } else {
                        hide_pane_with_id(pid);
                        self.managed.insert(kind.to_string(), (pane_id, true));
                    }
                }
                return;
            } else {
                self.managed.remove(kind);
            }
        }

        let cwd = self.get_editor_cwd();
        let new_id = match kind {
            "float" => self.open_float(&cwd),
            "vertical" => self.open_vertical(&cwd),
            "horizontal" => self.open_horizontal(&cwd),
            _ => return,
        };

        if let Some(id) = new_id {
            self.managed.insert(kind.to_string(), (id, false));
        }
    }

    fn pane_exists(&self, id: u32) -> bool {
        self.pane_manifest
            .panes
            .values()
            .flat_map(|v| v.iter())
            .any(|p| !p.is_plugin && p.id == id)
    }

    fn open_float(&self, path: &str) -> Option<u32> {
        let coords = FloatingPaneCoordinates::new(
            Some("5%".into()),
            Some("5%".into()),
            Some("90%".into()),
            Some("90%".into()),
            None,
            None,
        );
        let id = open_terminal_floating(path, coords).and_then(|pid| match pid {
            PaneId::Terminal(id) => Some(id),
            _ => None,
        })?;
        rename_terminal_pane(id, "Float");
        Some(id)
    }

    fn open_vertical(&self, path: &str) -> Option<u32> {
        let id = open_terminal(path).and_then(|pid| match pid {
            PaneId::Terminal(id) => Some(id),
            _ => None,
        })?;
        rename_terminal_pane(id, "Quake");

        Some(id)
    }

    fn open_horizontal(&self, path: &str) -> Option<u32> {
        let coords = FloatingPaneCoordinates::new(
            Some("0%".into()),
            Some("60%".into()),
            Some("100%".into()),
            Some("40%".into()),
            None,
            None,
        );
        let id = open_terminal_floating(path, coords).and_then(|pid| match pid {
            PaneId::Terminal(id) => Some(id),
            _ => None,
        })?;
        rename_terminal_pane(id, "Scratch");
        Some(id)
    }
}
