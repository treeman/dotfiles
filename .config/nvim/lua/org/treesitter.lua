local M = {}

---@param query string
---@return
function M.collect_captures(query)
  query = vim.treesitter.query.parse("djot", query)
  local parser = vim.treesitter.get_parser(0, "djot")

  local res = {}
  for _, tree in ipairs(parser:trees()) do
    local root = tree:root()
    for _, node, _ in query:iter_captures(root, 0) do
      table.insert(res, node)
    end
  end
  return res
end

---@param node TSNode
---@return TSNode | nil
function M.reparse_and_get_node(node)
  local row_start, col_start, row_end, _ = vim.treesitter.get_node_range(node)
  local parser = vim.treesitter.get_parser()
  parser:parse({ row_start, row_end })
  return vim.treesitter.get_node({ pos = { row_start, col_start } })
end

---@param node_type string | table
---@return TSNode | nil
function M.find_node(node, node_type)
  local curr = node
  while curr do
    if type(node_type) == "string" then
      if curr:type() == node_type then
        return curr
      end
    elseif type(node_type) == "table" then
      if node_type[curr:type()] then
        return curr
      end
    end

    curr = curr:parent()
  end

  return nil
end

---@param node_type string | table
---@param args? table
---@return TSNode | nil
function M.find_node_from_cursor(node_type, args)
  args = args or {}
  args["lang"] = "djot"

  local curr = vim.treesitter.get_node(args)
  return M.find_node(curr, node_type)
end

---@param node TSNode
---@param start_offset? integer
---@param end_offset? integer
function M.get_range(node, start_offset, end_offset)
  start_offset = start_offset or 0
  end_offset = end_offset or 0

  local row_start, col_start, row_end, col_end = vim.treesitter.get_node_range(node)
  return row_start, col_start + start_offset, row_end, col_end - end_offset
end

---@param node TSNode
---@param start_offset? integer
---@param end_offset? integer
function M.get_text(node, start_offset, end_offset)
  local row_start, col_start, row_end, col_end = M.get_range(node, start_offset, end_offset)
  return vim.api.nvim_buf_get_text(0, row_start, col_start, row_end, col_end, {})[1]
end

---@return TSNode | nil
function M.find_block_element_from_cursor()
  return M.find_node_from_cursor({
    table_caption = true,
    table_cell = true,
    heading_content = true,
    paragraph = true,
  })
end

---@param selection table
---@return boolean
function M.inside_block_element(selection)
  local block = M.find_block_element_from_cursor()
  return block ~= nil and vim.treesitter.node_contains(block, selection)
end

-- This filters rows on the same row as the target
local function filter_nodes_by_row(nodes, target_row)
  local curr_nodes = {}

  for _, node in ipairs(nodes) do
    local row_start, _, row_end, _ = vim.treesitter.get_node_range(node)

    if row_start <= target_row and row_end >= target_row then
      table.insert(curr_nodes, node)
    end
  end

  return curr_nodes
end

---@param nodes TSNode[]
---@param target_col integer
---@return TSNode | nil
local function filter_nodes_by_col(nodes, target_col)
  local curr_col_dist
  local curr_node

  for _, node in ipairs(nodes) do
    local _, col_start, _, col_end = vim.treesitter.get_node_range(node)

    -- Yeah this isn't maybe exact when links wrap multiple lines
    local target_dist = math.min(math.abs(target_col - col_start), math.abs(target_col - col_end))

    if not curr_col_dist or target_dist <= curr_col_dist then
      curr_node = node
      curr_col_dist = target_dist
    end
  end

  return curr_node
end

---@param nodes TSNode[]
---@return TSNode | nil
function M.get_nearest_node(nodes)
  -- (1, 0)-indexed
  local cursor = vim.api.nvim_win_get_cursor(0)
  local cursor_row = cursor[1] - 1
  local cursor_col = cursor[2]

  local by_row = filter_nodes_by_row(nodes, cursor_row)

  if #by_row == 1 then
    return by_row[1]
  end

  return filter_nodes_by_col(by_row, cursor_col)
end

return M
