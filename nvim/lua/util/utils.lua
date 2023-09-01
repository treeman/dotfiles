print("loading utils")

local M = {}

M.create_cmd = function(command, f, opts)
  opts = opts or {}
  vim.api.nvim_create_user_command(command, f, opts)
end

return M
