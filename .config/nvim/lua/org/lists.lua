local ts = require("org.treesitter")

local M = {}

local function get_marker_type(list)
  return list:child(0):child(0):type()
end

local function get_order_type(marker_type)
  local order_types = { "decimal", "lower_alpha", "upper_alpha", "lower_roman", "upper_roman" }

  for _, order_type in ipairs(order_types) do
    if string.find(marker_type, order_type) then
      return order_type
    end
  end
end

local function to_roman(num)
  local numerals = {
    { 1000, "M" },
    { 900, "CM" },
    { 500, "D" },
    { 400, "CD" },
    { 100, "C" },
    { 90, "XC" },
    { 50, "L" },
    { 40, "XL" },
    { 10, "X" },
    { 9, "IX" },
    { 5, "V" },
    { 4, "IV" },
    { 1, "I" },
  }

  local res = ""
  for _, pair in ipairs(numerals) do
    while num >= pair[1] do
      res = res .. pair[2]
      num = num - pair[1]
    end
  end
  return res
end

local function to_base26(num)
  local res = ""
  while num > 0 do
    num = num - 1 -- Adjust for zero-based index
    local remainder = num % 26
    res = string.char(remainder + string.byte("A")) .. res
    num = math.floor(num / 26)
  end
  return res
end

local function get_marker_replacement(i, order_type)
  if order_type == "decimal" then
    return tostring(i)
  elseif order_type == "lower_alpha" then
    return string.lower(to_base26(i))
  elseif order_type == "upper_alpha" then
    return to_base26(i)
  elseif order_type == "lower_roman" then
    return string.lower(to_roman(i))
  elseif order_type == "upper_roman" then
    return to_roman(i)
  end
end

local function set_list_item_marker(list_item, i, order_type)
  local marker = list_item:child(0)
  local row_start, col_start, row_end, col_end = vim.treesitter.get_node_range(marker)

  -- Skip ending `. ` or `) `
  col_end = col_end - 2

  local marker_text = vim.api.nvim_buf_get_text(0, row_start, col_start, row_end, col_end, {})[1]

  local new_marker = get_marker_replacement(i, order_type)

  local replacement = string.gsub(marker_text, "^(%s*%(?).-$", "%1" .. new_marker)

  vim.api.nvim_buf_set_text(0, row_start, col_start, row_end, col_end, { replacement })
  -- Join all edits into a single undo operation.
  vim.cmd("undojoin")
end

function M.reset_list_numbering()
  local list = ts.find_node_from_cursor("list")
  if not list then
    return
  end

  local marker_type = get_marker_type(list)
  local order_type = get_order_type(marker_type)
  if not order_type then
    return
  end

  for i = 1, list:child_count() do
    local list_item = list:child(i - 1)
    if not list_item then
      break
    end

    set_list_item_marker(list_item, i, order_type)
  end
end

return M
