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

function M.reset_list_numbering()
  local list = ts.find_node("list")
  if not list then
    return
  end

  local marker_type = get_marker_type(list)
  local order_type = get_order_type(marker_type)
  if not order_type then
    return
  end

  -- if list_type == "list_marker_decimal_period" then
  --
  -- end

  --
  -- $.list_marker_decimal_period,
  -- $.list_marker_lower_alpha_period,
  -- $.list_marker_upper_alpha_period,
  -- $.list_marker_lower_roman_period,
  -- $.list_marker_upper_roman_period,
  --
  -- $.list_marker_decimal_paren,
  -- $.list_marker_lower_alpha_paren,
  -- $.list_marker_upper_alpha_paren,
  -- $.list_marker_lower_roman_paren,
  -- $.list_marker_upper_roman_paren,
  --
  -- $.list_marker_decimal_parens,
  -- $.list_marker_lower_alpha_parens,
  -- $.list_marker_upper_alpha_parens,
  -- $.list_marker_lower_roman_parens,
  -- $.list_marker_upper_roman_parens,
  vim.notify("Order type: " .. order_type)
end

return M
