local wezterm = require 'wezterm'
local M = {}

function M.get_keys(funcs)
  return {
    { key = "a",  mods = "ALT", action = wezterm.action.ActivatePaneDirection 'Left' },
    { key = "d",  mods = "ALT", action = wezterm.action.ActivatePaneDirection 'Right' },
    { key = "w",  mods = "ALT", action = wezterm.action.ActivatePaneDirection 'Up' },
    { key = "s",  mods = "ALT", action = wezterm.action.ActivatePaneDirection 'Down' },
    { key = '\\', mods = 'ALT', action = funcs.smart_split("Bottom") },
    { key = 'h',  mods = 'ALT', action = funcs.smart_split("Right") },
    {
      key = 'B',
      mods = 'CTRL|SHIFT',
      action = wezterm.action_callback(function(window, pane)
        local raw_cwd = pane:get_current_working_dir()
        local cwd = raw_cwd and tostring(raw_cwd.file_path) or ""
        local history = funcs.load_history()
        local choices = {}

        for _, cmd in ipairs(history[cwd] or {}) do
          table.insert(choices, { label = cmd })
        end
        table.insert({ label = "(Type New Command...)" }, choices)

        window:perform_action(wezterm.action.InputSelector {
          title = "Compile: " .. cwd,
          choices = choices,
          action = wezterm.action_callback(function(iw, ip, id, label)
            if not label then return end

            if label == "(Type New Command...)" then
              iw:perform_action(wezterm.action.PromptInputLine {
                description = "Enter command (default: make):",
                action = wezterm.action_callback(function(w, p, line)
                  local cmd = (line == "" or not line) and "make" or line
                  funcs.save_history(cwd, cmd)
                  p:split { direction = 'Down', size = 0.3, cwd = raw_cwd, args = { 'bash', '-c', cmd .. ' 2>&1 | gobuild' } }
                end),
              }, ip)
            else
              funcs.save_history(cwd, label)
              ip:split { direction = 'Down', size = 0.3, cwd = raw_cwd, args = { 'bash', '-c', label .. ' 2>&1 | gobuild' } }
            end
          end),
        }, pane)
      end),
    },
  }
end

return M
