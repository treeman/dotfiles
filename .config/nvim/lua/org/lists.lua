local ts = require("org.treesitter")

local M = {}

local function get_marker_type(list)
  return list:child(0):child(0)
end

local function get_order_type(marker_type)
  -- decimal
  -- lower_alpha
  -- upper_alpha
  -- lower_roman
  -- upper_roman
  if string.find(marker_type, "decimal") then
    return "decimal"
  elseif string.find(marker_type, "lower_alpha") then
    return "lower_alpha"
  end
end

function M.reset_list_numbering()
  local list = ts.find_node("list")
  local marker_type = get_marker_type(list)
  local order_type = get_order_type(marker_type)

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
  -- vim.notify("List type: " .. list_type)
end

return M
