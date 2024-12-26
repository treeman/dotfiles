local M = {}

function M.find_node(node_type, lang)
  lang = lang or "djot"

  local curr = vim.treesitter.get_node({ lang = lang })
  while curr do
    if curr:type() == node_type then
      return curr
    end
    curr = curr:parent()
  end

  return nil
end

return M
