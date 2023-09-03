local function init()
	local map = vim.keymap.set
	local normal_keyboard = require("util").has_normal_keyboard()

	-- Copy/paste to mouse clipboard quickly
	map("n", "<leader>p", '"*p', { silent = true })
	map("n", "<leader>P", '"*P', { silent = true })
	map("n", "<leader>y", '"*y', { silent = true })
	map("n", "<leader>Y", '"*Y', { silent = true })
	-- Don't overwrite register when pasting in visual selection
	map("x", "p", '"_dP')

	-- Use ( as [ everywhere for T-34 that has ()
	-- on the base layer, but [] are hidden.
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

	-- Move visual lines
	map("n", "<up>", "gk")
	map("n", "<down>", "gj")

	-- Remaps to try from theprimaegen
	-- Don't move cursor when joining lines
	map("n", "J", "mzJ`z")
	-- Keep cursor in the middle when paging
	map("n", "<C-d>", "<C-d>zz")
	map("n", "<C-u>", "<C-u>zz")
	map("n", "<PageUp>", "<PageUp>zz")
	map("n", "<PageDown>", "<PageDown>zz")
	-- Keep search terms in the middle
	map("n", "n", "nzzzv")
	map("n", "N", "Nzzzv")

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
end

-- Keymaps that are sent to plugins during configuration
-- Return a module here so we can keep all keymap definitions
-- in the same place.
-- This is only for global keys, other contexts
-- (such as in the cmp selection menu) may be defined
-- in their respective files.
local M = {}

-- Wrap it in a function to prevent requiring this file evaluates
-- global keymaps multiple times.
M.init = init

-- These are in visual mode
M.textsubjects = {
	keymaps = {
		["."] = "textsubjects-smart",
		[";"] = "textsubjects-container-outer",
		["i;"] = "textsubjects-container-inner",
	},
	prev_selection = ",",
}

M.global_lsp = function()
	local map = vim.keymap.set
	local opts = { silent = true }
	map("n", "]d", vim.diagnostic.goto_next, opts)
	map("n", "[d", vim.diagnostic.goto_prev, opts)
end

M.buf_lsp = function(_, buffer)
	local map = vim.keymap.set
	local opts = { silent = true, buffer = buffer }
	-- FIXME there are other cool possibilities listed in nvim-lspconfig
	map("n", "<localleader>D", vim.lsp.buf.declaration, opts)
	map("n", "<localleader>d", vim.lsp.buf.definition, opts)
	map("n", "<localleader>r", vim.lsp.buf.references, opts)
	map("n", "<localleader>i", vim.lsp.buf.implementation, opts)
	map("n", "<localleader>t", vim.lsp.buf.type_definition, opts)
	map("n", "<localleader>h", vim.lsp.buf.hover, opts)
	map("n", "<localleader>s", vim.lsp.buf.signature_help, opts)
	map("n", "<localleader>x", vim.lsp.buf.code_action, opts)
	-- map("n", prefix .. "l", "<cmd>lua vim.diagnostic.open_float({ focusable = false })<CR>")
	map("n", "<localleader>ar", vim.lsp.buf.rename, opts)
	map("n", "<localleader>I", vim.lsp.buf.incoming_calls, opts)
	map("n", "<localleader>O", vim.lsp.buf.outgoing_calls, opts)
	map("n", "<localleader>w", vim.lsp.buf.document_symbol, opts)
	map("n", "<localleader>W", vim.lsp.buf.workspace_symbol, opts)

	-- Trouble is okay... But we really don't want it to steal focus!
	-- map("n", prefix .. "r", "<cmd>TroubleToggle lsp_references<CR>")
	-- map("n", prefix .. "d", "<cmd>TroubleToggle lsp_definitions<CR>")
	-- map("n", prefix .. "e", "<cmd>TroubleToggle document_diagnostics<CR>")
	-- map("n", prefix .. "w", "<cmd>TroubleToggle workspace_diagnostics<CR>")
end

M.gitsigns = function(buffer)
	local gitsigns = package.loaded.gitsigns
	local map = vim.keymap.set
	local opts = { silent = true, buffer = buffer }
	map("n", "]h", gitsigns.next_hunk, opts)
	map("n", "[h", gitsigns.prev_hunk, opts)
	map("n", "<leader>hs", gitsigns.stage_hunk, opts)
	map("n", "<leader>hr", gitsigns.reset_hunk, opts)
	map("v", "<leader>hs", function()
		gitsigns.stage_hunk({ vim.fn.line("."), vim.fn.line("v") })
	end, opts)
	map("v", "<leader>hr", function()
		gitsigns.reset_hunk({ vim.fn.line("."), vim.fn.line("v") })
	end, opts)
	map("n", "<leader>hb", function()
		gitsigns.blame_line({ full = true })
	end, opts)
end

return M
