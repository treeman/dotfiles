local ts = require("org.treesitter")

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
    vim.notify("Bad marker! " .. node:type(), vim.log.levels.ERROR)
    return
  end

  local check = node:child(0)
  local row_start, col_start, row_end, col_end = vim.treesitter.get_node_range(check)
  vim.api.nvim_buf_set_text(0, row_start, col_start + 1, row_end, col_end - 1, { marker })
end

local function get_marker_checkmark(node)
  if node:type() ~= "list_marker_task" then
    vim.notify("Bad marker! " .. node:type(), vim.log.levels.ERROR)
    return
  end

  local check = node:child(0)
  if check:type() == "unchecked" then
    return " "
  else
    return "x"
  end
end

local function toggle_marker(node)
  if node:type() ~= "list_marker_task" then
    vim.notify("Bad marker! " .. node:type(), vim.log.levels.ERROR)
    return
  end

  local mark
  if get_marker_checkmark(node) == "x" then
    mark = " "
  else
    mark = "x"
  end

  set_marker(node, mark)
  return mark
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

local function collect_child_markers(list)
  local res = {}
  for list_item in list:iter_children() do
    local list_type = list_item:named_child(0)
    if list_type and list_type:type() == "list_marker_task" then
      local mark = get_marker_checkmark(list_type)
      table.insert(res, mark)
    end
  end
  return res
end

local function all_checked(list)
  for _, val in ipairs(list) do
    if val ~= "x" and val ~= "X" then
      return false
    end
  end
  return true
end

local function update_parent_marker(child_marker)
  local list = ts.find_node(child_marker, "list")
  if not list then
    vim.notify("Couldn't find list", vim.log.levels.WARN)
    return
  end

  local marker = find_list_marker_up(list)
  if not marker then
    return
  end

  -- Collect the status of all tasks in the main list
  local markers = collect_child_markers(list)
  if all_checked(markers) then
    set_marker(marker, "x")
  else
    set_marker(marker, " ")
  end

  marker = ts.reparse_and_get_node(marker)
  update_parent_marker(marker)
end

local M = {}

M.toggle_task_marker = function()
  local parser = vim.treesitter.get_parser()
  if not parser then
    return
  end

  -- First find the list marker wrapping our cursor
  local marker = find_list_marker()
  if not marker then
    vim.notify("Failed to find task list", vim.log.levels.DEBUG)
    return
  end

  -- Toggle the marker
  local checked_value = toggle_marker(marker)

  -- Force treesitter update because the AST has changed.
  marker = ts.reparse_and_get_node(marker)
  if not marker then
    return
  end

  -- Set all the markers inside this lists content to the same value as we just set.
  local content = marker:next_sibling()
  if content and content:type() == "list_item_content" then
    toggle_markers_inside(content, checked_value)
  end

  update_parent_marker(marker)
end

return M
