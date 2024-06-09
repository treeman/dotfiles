vim.cmd("setlocal ts=2 sts=2 sw=2 expandtab")
-- Write and source lua file
vim.keymap.set("n", "<leader>x", ":write<CR>:source %<CR>")
