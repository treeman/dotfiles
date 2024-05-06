local diagnostics = require("blog.diagnostics")
local keymaps = require("config.keymaps")
local path = require("blog.path")
local server = require("blog.server")

local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup

local blog_group = augroup("blog", { clear = true })
local autocmd_pattern = path.blog_path .. "*.dj"

autocmd({ "BufRead", "BufNewFile" }, {
  pattern = autocmd_pattern,
  group = blog_group,
  callback = function(opts)
    vim.b[0].blog_file = true
    vim.api.nvim_set_current_dir(path.blog_path)
    server.establish_connection(true)
    keymaps.buf_blog(opts.buf)
    diagnostics.request_diagnostics_curr_buf()
  end,
})

-- This fun little thing tries to connect to my blogging
-- watch server and sends it document positions on move.
local function update_position()
  local pos = vim.api.nvim_win_get_cursor(0)

  server.cast({
    id = "CursorMoved",
    -- context = vim.fn.getline("."),
    linenum = pos[1],
    linecount = vim.fn.line("$"),
    column = pos[2],
    path = vim.fn.expand("%:p"),
  })
end

autocmd("CursorMoved", {
  pattern = autocmd_pattern,
  group = blog_group,
  callback = update_position,
})
