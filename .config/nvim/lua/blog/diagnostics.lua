local helpers = require("util.helpers")

M = {}

-- M.add_diagnostics_tmp = function(msg)
--   -- for _, buf in ipairs(util.list_buffers()) do
--   -- 	local buf_name = vim.api.nvim_buf_get_name(0)
--   -- 	if buf_name == "/home/tree/dotfiles/.config/nvim/lua/blog/diagnostics.lua" then
--   local diagnostics = {
--     {
--       col = 2,
--       end_col = 17,
--       lnum = 30,
--       end_lnum = 30,
--       message = "Link to non-existent link definition: `tag`",
--       severity = vim.diagnostic.severity.ERROR,
--     },
--   }
--
--   vim.diagnostic.set(vim.api.nvim_create_namespace("blog"), 0, diagnostics)
--   -- 	end
--   -- end
-- end

M.request_diagnostics_curr_buf = function()
  require("blog.server").cast({
    id = "RefreshDiagnostics",
    path = vim.fn.expand("%:p"),
  })
end

M.add_diagnostics = function(msg)
  for _, buf in ipairs(helpers.list_buffers()) do
    local buf_name = vim.api.nvim_buf_get_name(buf)
    local buf_diagnostics = msg[buf_name]

    if buf_diagnostics then
      vim.diagnostic.set(vim.api.nvim_create_namespace("blog"), buf, buf_diagnostics)
    end
  end
end

-- local msg = {
--   ["/home/tree/dotfiles/.config/nvim/lua/blog/diagnostics.lua"] = {
--     -- ["/path/to/file.dj"] = {
--     {
--       col = 2,
--       end_col = 17,
--       lnum = 30,
--       end_lnum = 30,
--       message = "Link to non-existent link definition: `tag`",
--       severity = vim.diagnostic.severity.WARN,
--     },
--   },
-- }
--
-- M.add_diagnostics(msg)

return M
