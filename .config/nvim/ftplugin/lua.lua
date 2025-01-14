local set = vim.opt_local

set.shiftwidth = 2
set.ts = 2
set.sts = 2
set.sw = 2
set.expandtab = true

-- Write and source lua file
vim.keymap.set("n", "<leader>w", ":write<CR>:source %<CR>")
require("config.keymaps").neotest(0)
