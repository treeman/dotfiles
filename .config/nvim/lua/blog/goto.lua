local server = require("blog.server")

local M = {}

M.goto_def = function()
	local pos = vim.api.nvim_win_get_cursor(0)

	server.call({
		id = "GotoDef",
		linenum = pos[1],
		column = pos[2],
		path = vim.fn.expand("%:p"),
	}, function(reply)
		if reply.path ~= vim.NIL or reply.linenum ~= vim.NIL then
			vim.cmd(":normal m'")
		end

		if reply.path ~= vim.NIL then
			vim.cmd(":e" .. reply.path)
		end

		if reply.linenum ~= vim.NIL then
			vim.api.nvim_win_set_cursor(0, { reply.linenum, reply.column })
		end
	end)
end

M.hover = function() end

return M
