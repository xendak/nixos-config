local get_state = ya.sync(function()
    local h = cx.active.current.hovered
    if not h then return nil end
    return {
        is_dir = h.cha.is_dir,
        url = tostring(h.url)
    }
end)

local function entry()
    local state = get_state()
    if not state then return end

    if state.is_dir then
        ya.emit("enter", {})
    else
        ya.emit("open", { state.url })
    end
end

return { entry = entry }
