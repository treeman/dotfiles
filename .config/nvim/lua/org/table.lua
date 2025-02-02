local ts = require("org.treesitter")

local M = {}

local function find_table_cells()
  local query = [[
  (table_cell) @cell
  ]]

  return ts.collect_captures(query)
end

function M.get_nearest_table_cell()
  return ts.get_nearest_node(find_table_cells())
end

function M.select_table_cell()
  local cell = M.get_nearest_table_cell()

  if not cell then
    return
  end

  local row_start, col_start, row_end, col_end = ts.get_range(cell)
  vim.api.nvim_buf_set_mark(0, "<", row_start + 1, col_start, {})
  vim.api.nvim_buf_set_mark(0, ">", row_end + 1, col_end - 1, {})
  vim.cmd("normal! gv")
end

return M
