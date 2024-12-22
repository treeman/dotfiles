local cmd = require("util.helpers").create_cmd
local map = vim.keymap.set
local log = require("plenary.log").new({
  plugin = "org",
  level = "debug",
})

local function find_list_marker_line()
  local task_link_query = [[
(list_item (list_marker_task (unchecked)) @unchecked) @list
(list_item (list_marker_task (checked)) @checked) @list
]]

  -- 1-indexed
  local cursor_row = vim.api.nvim_win_get_cursor(0)[1]

  local query = vim.treesitter.query.parse("djot", task_link_query)
  local parser = vim.treesitter.get_parser()

  for _, tree in ipairs(parser:trees()) do
    local root = tree:root()
    for _, node, _ in query:iter_captures(root, 0) do
      if node:type() == "list_marker_task" then
        -- 0-indexed
        local row_start, _, _, _ = vim.treesitter.get_node_range(node)
        if row_start + 1 == cursor_row then
          return node
        end
      end
    end
  end
end

local function find_list_marker_up(node)
  if node:type() == "list_marker_task" then
    return node
  end
  if node:type() == "list_item" then
    local type_node = node:child(0)
    if type_node:type() == "list_marker_task" then
      return type_node
    end
  end

  local parent = node:parent()
  if parent then
    return find_list_marker_up(parent)
  end
end

local function find_list_marker()
  -- This finds a checkbox on the same line.
  local marker = find_list_marker_line()
  if marker then
    return marker
  end

  -- This searches upwards for the checkbox.
  local current = vim.treesitter.get_node()
  if current then
    return find_list_marker_up(current)
  end
end

local function set_marker(node, marker)
  if node:type() ~= "list_marker_task" then
    log.debug("Bad marker! " .. node:type())
    return
  end

  local check = node:child(0)
  local row_start, col_start, row_end, col_end = vim.treesitter.get_node_range(check)
  vim.api.nvim_buf_set_text(0, row_start, col_start + 1, row_end, col_end - 1, { marker })
end

local function toggle_marker(node)
  if node:type() ~= "list_marker_task" then
    log.debug("Bad marker! " .. node:type())
    return
  end

  local check = node:child(0)
  local row_start, col_start, row_end, col_end = vim.treesitter.get_node_range(check)
  local replacement = " "
  if check:type() == "unchecked" then
    replacement = "x"
  end

  vim.api.nvim_buf_set_text(0, row_start, col_start + 1, row_end, col_end - 1, { replacement })

  return replacement
end

local function toggle_markers_inside(node, content)
  if node:type() == "list_marker_task" then
    set_marker(node, content)
  else
    for child in node:iter_children() do
      toggle_markers_inside(child, content)
    end
  end
end

local function toggle_list()
  local parser = vim.treesitter.get_parser()
  if not parser then
    return
  end

  -- First find the list marker wrapping our cursor
  local marker = find_list_marker()
  if not marker then
    log.debug("Failed to find task list")
    return
  end

  -- Toggle the marker
  local checked_value = toggle_marker(marker)

  -- Set all the markers inside this lists content to the same value as we just set.
  local content = marker:next_sibling()
  if not content or content:type() ~= "list_item_content" then
    return
  end
  toggle_markers_inside(content, checked_value)
end

cmd("ToggleList", toggle_list)

map("n", "<leader>x", toggle_list)
-- map("n", "<leader>e", make_link)
-- map("n", "<leader>e", goto_rel_file)
