$env.config.keybindings = [
  {
    name: select_all_line
    modifier: control
    keycode: char_a
    mode: [emacs, vi_insert, vi_normal]
    event: {
      send: executehostcommand
      cmd: "commandline set-cursor 0; commandline edit --insert (commandline | str length)"
    }
  }
  {
    name: yank_selection_to_clipboard
    modifier: alt
    keycode: char_y
    mode: [emacs, vi_insert, vi_normal]
    event: {
      send: executehostcommand
      cmd: "commandline get-selection | wl-copy"
    }
  }
  {
    name: yank_line_to_clipboard
    modifier: control
    keycode: char_y
    mode: [emacs, vi_insert, vi_normal]
    event: {
      send: executehostcommand
      cmd: "commandline | wl-copy"
    }
  }
]
