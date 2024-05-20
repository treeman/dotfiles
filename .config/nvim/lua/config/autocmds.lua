local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup

local group = augroup("autocmds.lua", { clear = true })

autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank()
  end,
  desc = "Briefly highlight yanked text",
  group = group,
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
