local map = vim.keymap.set

-- Some special mappings for my T-34 layout shenanigans
local normal_keyboard = os.getenv("NORMAL_KEYBORD") == 1

-- Copy/paste to mouse clipboard quickly
map("n", "<leader>p", '"*p', { silent = true })
map("n", "<leader>P", '"*P', { silent = true })
map("n", "<leader>y", '"*y', { silent = true })
map("n", "<leader>Y", '"*Y', { silent = true })
-- Don't overwrite register when pasting in visual selection
map("x", "p", '"_dP')

-- Use ( as a toggle prefix everywhere
map("n", "(", "[", { remap = true })
map("n", ")", "]", { remap = true })
map("o", "(", "[", { remap = true })
map("o", ")", "]", { remap = true })
map("x", "(", "[", { remap = true })
map("x", ")", "]", { remap = true })

-- Happy windows switching
if normal_keyboard then
	map("n", "<C-h>", "<c-w>h")
	map("n", "<C-j>", "<c-w>j")
	map("n", "<C-k>", "<c-w>k")
	map("n", "<C-l>", "<c-w>l")
else
	map("n", "<C-left>", "<c-w>h")
	map("n", "<C-down>", "<c-w>j")
	map("n", "<C-up>", "<c-w>k")
	map("n", "<C-right>", "<c-w>l")
end

-- Maximize current buffer
map("n", "<C-w>m", ":MaximizerToggle<CR>", { silent = true })

-- Edit file with prefilled path from the current file
map("n", "<leader>ef", ":e <C-R>=expand('%:p:h') . '/'<CR>")

-- Find files
map("n", "<leader>f", ":Telescope find_files<CR>", { silent = true })
-- Find files relative to current file
map(
	"n",
	"<leader>F",
	":lua require('telescope.builtin').find_files( { cwd = vim.fn.expand('%:p:h') })<CR>",
	{ silent = true }
)
-- Find in files
-- map("n", "<leader>/", ":execute 'Rg ' . input('Rg/')<CR>", { silent = true })
map("n", "<leader>/", ":Telescope live_grep<CR>", { silent = true })

-- Goto previous buffer
map("n", "<leader>B", ":edit #<CR>")
-- Find from open buffers
map("n", "<leader>b", ":lua require('config.telescope_actions').open_buffer()<CR>", { silent = true })

map("n", "<leader>o", ":Telescope oldfiles<CR>", { silent = true })

-- Edit files in a buffer
map("n", "<leader>ed", ":Oil .<CR>")
-- Edit files in within the current directory
map("n", "<leader>eD", ":Oil <C-R>=expand('%:p:h')<CR><CR>")

map("n", "<leader>d", ":Neotree toggle=true<CR>")

-- Supercharged spell correction!
map("n", "z=", ":Telescope spell_suggest<CR>", { silent = true })

-- norg mapping

-- Open scratch file
map("n", "<leader>ss", ":e ~/norg/scratch.norg<CR>")
map("n", "<leader>n", ":lua require('config.telescope_actions').open_norg('')<CR>")
map("n", "<leader>ep", ":lua require('config.telescope_actions').open_norg('projects')<CR>")
map("n", "<leader>ea", ":lua require('config.telescope_actions').open_norg('areas')<CR>")
map("n", "<leader>er", ":lua require('config.telescope_actions').open_norg('resources')<CR>")
map("n", "<leader>eA", ":lua require('config.telescope_actions').open_norg('archive')<CR>")

-- Git
local neogit = require("neogit")
map("n", "gs", ":Neogit<CR>")
map("n", "g<space>", ":Git ")

local telescope_builtin = require("telescope.builtin")

map("n", "gb", telescope_builtin.git_branches)

-- Ideas
--require('telescope.builtin').git_commits()
--require('telescope.builtin').git_bcommits()
--require('telescope.builtin').git_bcommits_range()

-- TODO
-- Open all commits with gll (so we don't have to press gsll) ?
-- View history of a single file
-- View history of selection in a single file

-- nnoremap gs :Git<CR>
-- nnoremap g<space> :Git
-- nnoremap gll :Flogsplit<CR>
-- nnoremap glf :Flogsplit -path=%<CR>
-- xnoremap glf :Flogsplit -- --no-patch<CR>
-- nnoremap <silent> <leader>C :Commits<CR>
-- nnoremap gb :Telescope git_branches<CR>

-- " Fugitive Conflict Resolution
-- nnoremap gds :Gdiffsplit!<CR>
-- nnoremap gdh :diffget //2<CR>
-- nnoremap gdl :diffget //3<CR>
