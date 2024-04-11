local server = require("blog.server")

local M = {}

M.goto_def = function()
	server.call({
		id = "GotoDef",
		linenum = vim.fn.line("."),
		column = vim.fn.col("."),
		path = vim.fn.expand("%:p"),
	}, function(reply)
		if reply.path ~= vim.NIL or reply.linenum then
			vim.cmd("m'")
		end

		if reply.path ~= vim.NIL then
			vim.cmd(":e" .. reply.path)
		end

		if reply.linenum then
			vim.api.nvim_win_set_cursor(0, { reply.linenum, reply.column })
		end
	end)
end

M.hover = function() end

return M
