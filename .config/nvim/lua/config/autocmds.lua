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

local function open_org_display()
  -- TODO what info do I want to see?
  -- 1. A tree view with all the files or a list of the most recently opened files
  -- 2. Focus items
  -- 3. List active projects (should prune them regularly)
  -- 4. Goals
  -- 5. maybe.dj
  vim.cmd(":e ~/org/reviews.dj")
  vim.bo.filetype = "djot"
end

-- This opens the plan.dj every time we open Neovim
-- unless we start Neovim with an argument
autocmd("VimEnter", {
  callback = function()
    for i, arg in ipairs(vim.fn.argv()) do
      if arg == "--org" then
        open_org_display()
      end
    end
  end,
})
