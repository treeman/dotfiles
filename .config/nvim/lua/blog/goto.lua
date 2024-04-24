local server = require("blog.server")

local M = {}

M.goto_def = function()
	local pos = vim.api.nvim_win_get_cursor(0)

	server.call({
		id = "GotoDef",
		linenum = pos[1] - 1,
		column = pos[2],
		path = vim.fn.expand("%:p"),
	}, function(reply)
		if reply.path or reply.linenum then
			vim.cmd(":normal m'")
		end

		if reply.path then
			vim.cmd(":e" .. reply.path)
		end

		if reply.linenum then
			vim.api.nvim_win_set_cursor(0, { reply.linenum + 1, reply.column })
		end
	end)
end

return M
