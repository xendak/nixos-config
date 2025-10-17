-- Modified version of: https://github.com/AnirudhG07/plugins-yazi/blob/main/copy-file-contents.yazi/main.lua
local selected_files = ya.sync(function()
    local tab, paths = cx.active, {}
    for _, u in pairs(tab.selected) do
        paths[#paths + 1] = tostring(u)
    end
    if #paths == 0 and tab.current.hovered then
        paths[#paths + 1] = tostring(tab.current.hovered.url)
    end
    return paths
end)

local function copy_content()
    local files = selected_files()
    if #files == 0 then
        return
    end

    local all_contents = {}
    for _, file in ipairs(files) do
        local f = io.open(file, "r")
        if f then
            local content = f:read("*a")
            content = content:gsub("%s+$", "")
            table.insert(all_contents, content)
            f:close()
        end
    end

    if #all_contents > 0 then
        ya.clipboard(table.concat(all_contents, "\n"))
    end
end

-- Copies the content in a special format including file path and type.
-- Really good for AI's :)
-- Appends a newline character between each formatted block.
local function copy_formatted()
    local files = selected_files()
    if #files == 0 then
        return
    end

    local formatted_blocks = {}
    for _, file_path in ipairs(files) do
        local f = io.open(file_path, "r")
        if f then
            local file_content = f:read("*a")
            f:close()

            local extension = file_path:match("%.([^.]+)$") or ""

            local block = string.format(
                "%s\n```%s\n%s```",
                file_path,
                extension,
                file_content
            )
            table.insert(formatted_blocks, block)
        end
    end

    if #formatted_blocks > 0 then
        ya.clipboard(table.concat(formatted_blocks, "\n"))
    end
end

return {
    entry = function(self, job)
        local action = job.args[1]

        if action == "formatted" then
            copy_formatted()
        else
            copy_content()
        end
    end,
}
