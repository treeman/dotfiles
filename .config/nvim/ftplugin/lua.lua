vim.cmd("setlocal ts=2 sts=2 sw=2 expandtab")
-- Write and source lua file
vim.keymap.set("n", "<leader>w", ":write<CR>:source %<CR>")
require("config.keymaps").neotest(0)
