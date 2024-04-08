local path = require("blog.path")
local server = require("blog.server")
local keymaps = require("config.keymaps")

local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup

local blog_group = augroup("blog", { clear = true })
local autocmd_pattern = path.blog_path .. "*.{dj,markdown,md}"

autocmd({ "BufRead", "BufNewFile" }, {
	pattern = autocmd_pattern,
	group = blog_group,
	callback = function(opts)
		vim.api.nvim_set_current_dir(path.blog_path)
		server.establish_connection(true)
		keymaps.buf_blog(opts.buf)
	end,
})

autocmd("CursorMoved", {
	pattern = autocmd_pattern,
	group = blog_group,
	callback = server.update_position,
})
