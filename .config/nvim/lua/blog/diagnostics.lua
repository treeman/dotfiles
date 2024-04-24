local util = require("util")

M = {}

M.add_diagnostics = function(msg)
	for _, buf in ipairs(util.list_buffers()) do
		local buf_name = vim.api.nvim_buf_get_name(0)
		local buf_diagnostics = msg[buf_name]

		if buf_diagnostics then
			local diagnostics = {}
			for _, d in ipairs(buf_diagnostics) do
				-- The positions we send are 1-indexed, but diagnostics are 0-indexed...
				-- I hope this is fine...?
				-- table.insert(diagnostics, {
				-- 	lnum = d.lnum,
				-- 	end_lnum = d.end_lnum,
				-- 	col = d.col,
				-- 	end_col = d.end_col,
				-- 	message = d.message,
				-- 	severity = vim.diagnostic.severity.WARN,
				-- })
				table.insert(diagnostics, d)
			end

			vim.diagnostic.set(vim.api.nvim_create_namespace("blog"), buf, diagnostics)
		end
	end
end

return M
