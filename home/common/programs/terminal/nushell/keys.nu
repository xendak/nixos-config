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
  {
    name: fuzzy_history
    modifier: control
    keycode: char_r
    mode: [emacs, vi_normal, vi_insert]
    event: [
      {
        send: ExecuteHostCommand
        cmd: "commandline edit --insert (
          history
            | get command
            | reverse
            | uniq
            | str join (char -i 0)
            | fzf
              --preview '{}'
              --preview-window 'right:30%'
              --scheme history
              --read0
              --layout reverse
              --height 40%
              --query (commandline)
            | decode utf-8
            | str trim
        )"
      }
    ]
  }
]
