local get_targets = ya.sync(function()
    local urls = {}
    local selected = cx.active.selected
    
    local has_selection = false
    for _, url in pairs(selected) do
        table.insert(urls, tostring(url))
        has_selection = true
    end

    if not has_selection then
        local h = cx.active.current.hovered
        if h then
            table.insert(urls, tostring(h.url))
        end
    end
    
    return urls
end)

local function entry()
    local urls = get_targets()
    
    if #urls == 0 then 
        return ya.notify({ 
            title = "Shell", 
            content = "No targets found!", 
            level = "warn", 
            timeout = 3 
        })
    end

    local value, event = ya.input({
        title = "Execute on " .. #urls .. " items:",
        pos = { "top-center", y = 3, w = 40 },
    })

    if event ~= 1 or not value or value == "" then return end

    -- individual execution for each entry
    for _, url in ipairs(urls) do
        local single_cmd = string.format('%s %s', value, ya.quote(url))
        
        ya.emit("shell", {
            single_cmd,
            block = true,
            confirm = true
        })
    end
    
    ya.emit("escape", { visual = true })
end

return { entry = entry }
