local M = {}

---@param node_type string
---@param opts vim.treesitter.get_node.Opts?
---@return TSNode | nil
function M.find_node(node_type, opts)
  opts = opts or { lang = "djot" }

  local curr = vim.treesitter.get_node(opts)
  while curr do
    if curr:type() == node_type then
      return curr
    end
    curr = curr:parent()
  end

  return nil
end

---@param node TSNode
---@param start_offset? integer
---@param end_offset? integer
function M.get_text(node, start_offset, end_offset)
  start_offset = start_offset or 0
  end_offset = end_offset or 0

  local row_start, col_start, row_end, col_end = vim.treesitter.get_node_range(node)
  return vim.api.nvim_buf_get_text(
    0,
    row_start,
    col_start + start_offset,
    row_end,
    col_end - end_offset,
    {}
  )[1]
end

return M
