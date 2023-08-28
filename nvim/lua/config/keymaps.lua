-- Some special mappings for my T-34 layout shenanigans
local normal_keyboard = os.getenv("NORMAL_KEYBORD") == 1

-- Copy/paste to mouse clipboard quickly
vim.keymap.set("n", "<leader>p", '"*p', { silent = true })
vim.keymap.set("n", "<leader>P", '"*P', { silent = true })
vim.keymap.set("n", "<leader>y", '"*y', { silent = true })
vim.keymap.set("n", "<leader>Y", '"*Y', { silent = true })
-- Don't overwrite register when pasting in visual selection
vim.keymap.set("x", "p", '"_dP')

-- Use ( as a toggle prefix everywhere
vim.keymap.set("n", "(", "[", { remap = true })
vim.keymap.set("n", ")", "]", { remap = true })
vim.keymap.set("o", "(", "[", { remap = true })
vim.keymap.set("o", ")", "]", { remap = true })
vim.keymap.set("x", "(", "[", { remap = true })
vim.keymap.set("x", ")", "]", { remap = true })

-- Happy windows switching
if narmal_keyboard then
  vim.keymap.set("n", "<C-h>", "<c-w>h")
  vim.keymap.set("n", "<C-j>", "<c-w>j")
  vim.keymap.set("n", "<C-k>", "<c-w>k")
  vim.keymap.set("n", "<C-l>", "<c-w>l")
else
  vim.keymap.set("n", "<C-left>", "<c-w>h")
  vim.keymap.set("n", "<C-down>", "<c-w>j")
  vim.keymap.set("n", "<C-up>", "<c-w>k")
  vim.keymap.set("n", "<C-right>", "<c-w>l")
end

-- Maximize current buffer
vim.keymap.set("n", "<C-w>m", ":MaximizerToggle<CR>", { silent = true })


-- Edit file with prefilled path from the current file
vim.keymap.set("n", "<leader>ef", ":e <C-R>=expand('%:p:h') . '/'<CR>")


-- Find files
vim.keymap.set("n", "<leader>f", ":Telescope find_files<CR>", { silent = true })
-- Find files relative to current file
vim.keymap.set("n", "<leader>F", ":lua require('telescope.builtin').find_files( { cwd = vim.fn.expand('%:p:h') })<CR>", { silent = true })
-- Find in files
vim.keymap.set("n", "<leader>/", ":execute 'Rg ' . input('Rg/')<CR>", { silent = true })

-- Goto previous buffer
vim.keymap.set("n", "<leader>B", ":edit #<CR>")
-- Find from open buffers
vim.keymap.set("n", "<leader>b", ":lua require('config.telescope_actions').open_buffer()<CR>", { silent = true })

vim.keymap.set("n", "<leader>o", ":Telescope oldfiles<CR>", { silent = true})

-- Edit files in a buffer
vim.keymap.set("n", "<leader>ed", ":Oil .<CR>")
-- Edit files in within the current directory
vim.keymap.set("n", "<leader>eD", ":Oil <C-R>=expand('%:p:h')<CR><CR>")

-- Supercharged spell correction!
vim.keymap.set("n", "z=", ":Telescope spell_suggest<CR>", { silent = true })

-- norg mapping

-- Open scratch file
vim.keymap.set("n", "<leader>ss", ":e ~/norg/scratch.norg<CR>")
vim.keymap.set("n", "<leader>n", ":lua require('config.telescope_actions').open_norg('')<CR>")
vim.keymap.set("n", "<leader>ep", ":lua require('config.telescope_actions').open_norg('projects')<CR>")
vim.keymap.set("n", "<leader>ea", ":lua require('config.telescope_actions').open_norg('areas')<CR>")
vim.keymap.set("n", "<leader>er", ":lua require('config.telescope_actions').open_norg('resources')<CR>")
vim.keymap.set("n", "<leader>eA", ":lua require('config.telescope_actions').open_norg('archive')<CR>")
