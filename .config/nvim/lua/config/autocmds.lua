local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup

autocmd("TextYankPost", {
	callback = function()
		vim.highlight.on_yank()
	end,
	desc = "Briefly highlight yanked text",
	group = augroup("yank", { clear = true }),
})

-- FIXME use modelines instead?
local filegroup = augroup("filegroup", { clear = true })
autocmd("FileType", {
	pattern = "html",
	group = filegroup,
	command = "setlocal ts=2 sts=2 sw=2 expandtab",
})
autocmd("FileType", {
	pattern = "javascript",
	group = filegroup,
	command = "setlocal ts=2 sts=2 sw=2 noexpandtab",
})
autocmd("FileType", {
	pattern = "json",
	group = filegroup,
	command = "setlocal ts=2 sts=2 sw=2 expandtab",
})

autocmd("FileType", {
	pattern = "lua",
	group = filegroup,
	command = "setlocal ts=2 sts=2 sw=2 noexpandtab",
})

autocmd("BufRead", {
	-- I do not want the filetype to be htmldjango when it contains a {% ... %} string.
	pattern = "*.html",
	group = filegroup,
	command = "set filetype=html",
})

-- Cursor line only in active window
local cursorlinegroup = augroup("cursorlinegroup", { clear = true })
autocmd({ "VimEnter", "WinEnter", "BufWinEnter" }, {
	group = cursorlinegroup,
	pattern = "*",
	callback = function(x)
		-- Not sure how to hide this from certain file buffers?
		-- Maybe we can query for the filetype of buffer, and then exclude some things?
		-- This ignores the dashboard at least, which maybe is good enough?
		if string.len(x.file) > 0 then
			-- print(vim.inspect(x))
			vim.opt_local.cursorline = true
		end
	end,
})
autocmd("WinLeave", {
	group = cursorlinegroup,
	pattern = "*",
	callback = function()
		vim.opt_local.cursorline = false
	end,
})

-- Workaround for: https://github.com/nvim-telescope/telescope.nvim/issues/2027
-- Should be fixed in a future Neovim relelase.
autocmd("WinLeave", {
	callback = function()
		if vim.bo.ft == "TelescopePrompt" and vim.fn.mode() == "i" then
			vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "i", false)
		end
	end,
})
