local wezterm = require 'wezterm'
local M = {}

M.history_path = "@HISTORY_PATH@"

function M.load_history()
  local f = io.open(M.history_path, "r")
  if f then
    local content = f:read("*a")
    f:close()
    return wezterm.json_decode(content) or {}
  end
  return {}
end

function M.save_history(dir, cmd)
  local history = M.load_history()
  if not history[dir] then history[dir] = {} end
  -- Move to top logic
  for i, v in ipairs(history[dir]) do
    if v == cmd then
      table.remove(history[dir], i)
      break
    end
  end
  table.insert(history[dir], 1, cmd)
  if #history[dir] > 20 then table.remove(history[dir]) end

  local f = io.open(M.history_path, "w")
  if f then
    f:write(wezterm.json_encode(history))
    f:close()
  end
end

function M.smart_split(direction)
  return wezterm.action_callback(function(_, pane)
    local tab = pane:tab()
    local panes = tab:panes_with_info()
    if #panes == 1 then
      pane:split({
        direction = direction,
        size = 0.35,
      })
    elseif not panes[1].is_zoomed then
      panes[1].pane:activate()
      tab:set_zoomed(true)
    else
      tab:set_zoomed(false)
      panes[2].pane:activate()
    end
  end)
end

return M
