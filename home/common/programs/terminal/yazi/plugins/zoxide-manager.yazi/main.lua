local get_cwd = ya.sync(function(_) return tostring(cx.active.current.cwd) end)

local function entry(_, job)
    local action = job.args[1]
    local cwd = get_cwd()
    local escaped = string.format("%q", cwd)

    local action_map = {
        add    = "zoxide add "    .. escaped,
        remove = "zoxide remove " .. escaped,
        reset  = "zoxide remove " .. escaped .. " ; zoxide add " .. escaped,
    }

    local cmd = action_map[action]
    if cmd then
        ya.emit("shell", {
            cmd,
            block = false,
            confirm = false
        })
        ya.notify({
            title   = "Zoxide",
            content = action:gsub("^%l", string.upper) .. ": " .. cwd,
            timeout = 2,
        })
    else
        ya.notify({
            title   = "Zoxide",
            content = "Unknown action: " .. tostring(action),
            level   = "error",
            timeout = 3,
        })
    end
end

return { entry = entry }
