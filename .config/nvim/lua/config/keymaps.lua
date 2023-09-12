local function init()
	local map = vim.keymap.set
	local normal_keyboard = require("util").has_normal_keyboard()

	-- Copy/paste to mouse clipboard quickly
	map("n", "<leader>p", '"*p', { silent = true, desc = "Paste from mouse" })
	map("n", "<leader>P", '"*P', { silent = true, desc = "Paste before from mouse" })
	map("n", "<leader>y", '"*y', { silent = true, desc = "Yank into mouse" })
	map("n", "<leader>Y", '"*Y', { silent = true, desc = "Yank line into mouse" })
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

	map("n", "]q", ":cnext<cr>", { desc = "Next quickfix" })
	map("n", "[q", ":cprevious<cr>", { desc = "Prev quickfix" })
	map("n", "]Q", ":clast<cr>", { desc = "Last quickfix" })
	map("n", "[Q", ":cfirst<cr>", { desc = "First quickfix" })

	map("n", "]l", ":lnext<cr>", { desc = "Next loclist" })
	map("n", "[l", ":lprevious<cr>", { desc = "Prev loclist" })
	map("n", "]L", ":llast<cr>", { desc = "Last loclist" })
	map("n", "[L", ":lfirst<cr>", { desc = "First loclist" })

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
	-- map("n", "<C-d>", "<C-d>zz")
	-- map("n", "<C-u>", "<C-u>zz")
	-- map("n", "<PageUp>", "<PageUp>zz")
	-- map("n", "<PageDown>", "<PageDown>zz")
	-- Keep search terms in the middle
	-- map("n", "n", "nzzzv")
	-- map("n", "N", "Nzzzv")
	-- TODO should probably try to align C-f, C-b / and all [] prefixes... So why bother?

	-- Maximize current buffer
	map("n", "<C-w>m", ":MaximizerToggle<CR>", { silent = true, desc = "Maximize window" })

	-- Edit file with prefilled path from the current file
	map("n", "<leader>ef", ":e <C-R>=expand('%:p:h') . '/'<CR>", { desc = "Edit relative file" })

	-- Goto previous buffer
	map("n", "<leader>B", ":edit #<CR>", { desc = "Previous buffer" })

	map("n", "<leader>o", ":Telescope oldfiles<CR>", { silent = true, desc = "Old files" })

	-- Edit files in a buffer
	map("n", "<leader>ed", ":Oil .<CR>", { desc = "Edit workspace" })
	-- Edit files in within the current directory
	map("n", "<leader>eD", ":Oil <C-R>=expand('%:p:h')<CR><CR>", { desc = "Edit relative workspace" })

	map("n", "<leader>d", ":Neotree toggle=true<CR>", { desc = "Neotree" })

	-- Supercharged spell correction!
	map("n", "z=", ":Telescope spell_suggest<CR>", { silent = true, desc = "Spell suggest" })

	-- norg mapping

	-- Open scratch file
	map("n", "<leader>es", ":e ~/norg/scratch.norg<CR>", { desc = "Scratch" })

	-- Git
	map("n", "gs", ":Neogit<CR>", { desc = "Git status" })
	map("n", "g<space>", ":Git ", { desc = "Git" })
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

M.telescope = function()
	local map = vim.keymap.set

	-- FIXME remove :lua from functions

	-- Find files
	map("n", "<leader>f", ":Telescope find_files<CR>", { silent = true, desc = "Find files" })
	-- Find files relative to current file
	map(
		"n",
		"<leader>F",
		":lua require('telescope.builtin').find_files( { cwd = vim.fn.expand('%:p:h') })<CR>",
		{ silent = true, desc = "Find relative file" }
	)

	-- Find in files
	map("n", "<leader>/", ":Telescope live_grep<CR>", { silent = true, desc = "Find in files" })

	-- Find from open buffers
	map(
		"n",
		"<leader>b",
		":lua require('config.telescope_actions').open_buffer()<CR>",
		{ silent = true, desc = "Buffers" }
	)

	-- Open norg files
	map("n", "<leader>n", ":lua require('config.telescope_actions').open_norg('')<CR>", { desc = "Neorg" })
	map(
		"n",
		"<leader>ep",
		":lua require('config.telescope_actions').open_norg('projects')<CR>",
		{ desc = "Neorg projects" }
	)
	map("n", "<leader>ea", ":lua require('config.telescope_actions').open_norg('areas')<CR>", { desc = "Neorg areas" })
	map(
		"n",
		"<leader>er",
		":lua require('config.telescope_actions').open_norg('resources')<CR>",
		{ desc = "Neorg resources" }
	)
	map(
		"n",
		"<leader>eA",
		":lua require('config.telescope_actions').open_norg('archive')<CR>",
		{ desc = "Neorg archive" }
	)

	map("n", "gb", require("telescope.builtin").git_branches, { desc = "Git branches" })

	-- Ideas
	--require('telescope.builtin').git_commits()
	--require('telescope.builtin').git_bcommits()
	--require('telescope.builtin').git_bcommits_range()
end

M.dial = function()
	local dial = require("dial.map")
	vim.keymap.set("n", "<C-a>", dial.inc_normal(), { noremap = true })
	vim.keymap.set("n", "<C-x>", dial.dec_normal(), { noremap = true })
end

-- These are in visual mode
M.textsubjects = {
	keymaps = {
		["."] = "textsubjects-smart",
		[";"] = "textsubjects-container-outer",
		["i;"] = "textsubjects-container-inner",
	},
	prev_selection = ",",
}

-- Maps four pairs:
-- [f, [F, ]f, ]F
-- for the given treesitter textobject
-- see: https://github.com/nvim-treesitter/nvim-treesitter-textobjects
local ts_move_keys = {
	f = { query = "@function.outer", desc = "goto function" },
	w = { query = "@parameter.outer", desc = "goto parameter" },
	a = { query = "@attribute.inner", desc = "goto attribute" },
	b = { query = "@block.inner", desc = "goto block" },
	c = { query = "@class.outer", desc = "goto class" },
	x = { query = "@comment.outer", desc = "goto comment" },
	s = { query = "@statement.outer", desc = "goto statement" },
}

M.ts_goto_next_start = {}
M.ts_goto_next_end = {}
M.ts_goto_previous_start = {}
M.ts_goto_previous_end = {}

for k, v in pairs(ts_move_keys) do
	M.ts_goto_next_start["]" .. k] = v
	M.ts_goto_next_end["]" .. string.upper(k)] = v
	M.ts_goto_previous_start["[" .. k] = v
	M.ts_goto_previous_end["[" .. string.upper(k)] = v
end

M.ts_swap_next = {
	["<leader>s"] = { query = "@parameter.inner", desc = "Swap next parameter" },
}
M.ts_swap_previous = {
	["<leader>S"] = { query = "@parameter.inner", desc = "Swap previous parameter" },
}

M.ts_select = {
	["af"] = { query = "@function.outer", desc = "Select outer function" },
	["if"] = { query = "@function.inner", desc = "Select inner function" },
	["ac"] = { query = "@class.outer", desc = "Select outer class" },
	["ic"] = { query = "@class.inner", desc = "Select inner class" },
	["ab"] = { query = "@block.outer", desc = "Select outer block" },
	["ib"] = { query = "@block.inner", desc = "Select inner block" },
	["aa"] = { query = "@attribute.outer", desc = "Select outer attribute" },
	["ia"] = { query = "@attribute.inner", desc = "Seect inner attribute" },
	["ax"] = { query = "@comment.outer", desc = "Select outer comment" },
	["ix"] = { query = "@comment.inner", desc = "Select inner comment" },
	["as"] = { query = "@statement.outer", desc = "Select outer statement" },
	["is"] = { query = "@statement.inner", desc = "Select inner statement" },
	["aw"] = { query = "@parameter.outer", desc = "Select outer parameter" },
	["iw"] = { query = "@parameter.inner", desc = "Select inner parameter" },
}

M.global_lsp = function()
	local map = vim.keymap.set
	map("n", "]d", vim.diagnostic.goto_next, { silent = true, desc = "Next diagnostic" })
	map("n", "[d", vim.diagnostic.goto_prev, { silent = true, desc = "Prev diagnostic" })
end

M.buf_lsp = function(_, buffer)
	local map = vim.keymap.set
	-- FIXME there are other cool possibilities listed in nvim-lspconfig
	map("n", "<localleader>D", vim.lsp.buf.declaration, { silent = true, buffer = buffer, desc = "Declaration" })
	map("n", "<localleader>d", vim.lsp.buf.definition, { silent = true, buffer = buffer, desc = "Definition" })
	-- map(
	-- 	"n",
	-- 	"<localleader>d",
	-- 	":TroubleToggle lsp_definitions<CR>",
	-- 	{ silent = true, buffer = buffer, desc = "Definitions" }
	-- )
	map("n", "<localleader>r", vim.lsp.buf.references, { silent = true, buffer = buffer, desc = "References" })
	-- map(
	-- 	"n",
	-- 	"<localleader>r",
	-- 	":TroubleToggle lsp_references<CR>",
	-- 	{ silent = true, buffer = buffer, desc = "References" }
	-- )
	map("n", "<localleader>i", vim.lsp.buf.implementation, { silent = true, buffer = buffer, desc = "Implementation" })
	map(
		"n",
		"<localleader>t",
		vim.lsp.buf.type_definition,
		{ silent = true, buffer = buffer, desc = "Type definition" }
	)
	map("n", "<localleader>h", vim.lsp.buf.hover, { silent = true, buffer = buffer, desc = "Hover" })
	map("n", "<localleader>s", vim.lsp.buf.signature_help, { silent = true, buffer = buffer, desc = "Signature help" })
	map("n", "<localleader>x", vim.lsp.buf.code_action, { silent = true, buffer = buffer, desc = "Code action" })
	-- map("n", prefix .. "l", "<cmd>lua vim.diagnostic.open_float({ focusable = false })<CR>")
	map("n", "<localleader>R", vim.lsp.buf.rename, { silent = true, buffer = buffer, desc = "Rename" })
	map("n", "<localleader>I", vim.lsp.buf.incoming_calls, { silent = true, buffer = buffer, desc = "Incoming calls" })
	map("n", "<localleader>O", vim.lsp.buf.outgoing_calls, { silent = true, buffer = buffer, desc = "Outgoing calls" })
	map(
		"n",
		"<localleader>w",
		vim.lsp.buf.document_symbol,
		{ silent = true, buffer = buffer, desc = "Document symbols" }
	)
	map(
		"n",
		"<localleader>W",
		vim.lsp.buf.workspace_symbol,
		{ silent = true, buffer = buffer, desc = "Workspace symbols" }
	)

	map(
		"n",
		"<localleader>e",
		":TroubleToggle document_diagnostics<CR>",
		{ silent = true, buffer = buffer, desc = "Document diagnostics" }
	)
	map(
		"n",
		"<localleader>E",
		":TroubleToggle workspace_diagnostics<CR>",
		{ silent = true, buffer = buffer, desc = "Workspace diagnostics" }
	)
end

M.gitsigns = function(buffer)
	local gitsigns = package.loaded.gitsigns
	local map = vim.keymap.set
	map("n", "]h", gitsigns.next_hunk, { silent = true, buffer = buffer, desc = "Next hunk" })
	map("n", "[h", gitsigns.prev_hunk, { silent = true, buffer = buffer, desc = "Prev hunk" })
	map("n", "<leader>hs", gitsigns.stage_hunk, { silent = true, buffer = buffer, desc = "Stage hunk" })
	map("n", "<leader>hr", gitsigns.reset_hunk, { silent = true, buffer = buffer, desc = "Reset hunk" })
	map("v", "<leader>hs", function()
		gitsigns.stage_hunk({ vim.fn.line("."), vim.fn.line("v") })
	end, { silent = true, buffer = buffer, desc = "Stage hunk" })
	map("v", "<leader>hr", function()
		gitsigns.reset_hunk({ vim.fn.line("."), vim.fn.line("v") })
	end, { silent = true, buffer = buffer, desc = "Reset hunk" })
	map("n", "<leader>hb", function()
		gitsigns.blame_line({ full = true })
	end, { silent = true, buffer = buffer, desc = "Blame hunk" })
end

M.marks = {
	set = "m",
	delete = "dm",
	delete_line = "dm-",
	delete_buf = "dm<space>",
	next = "]m",
	prev = "[m",
	preview = "m:",
}

M.trouble = function()
	local map = vim.keymap.set
	local trouble = require("trouble")
	map("n", "]t", function()
		trouble.next({ skip_groups = true, jump = true })
	end, { desc = "Next trouble" })
	map("n", "[t", function()
		trouble.previous({ skip_groups = true, jump = true })
	end, { desc = "Prev trouble" })
	map("n", "]T", function()
		trouble.last({ skip_groups = true, jump = true })
	end, { desc = "Last trouble" })
	map("n", "[T", function()
		trouble.first({ skip_groups = true, jump = true })
	end, { desc = "First trouble" })
	map("n", "<leader>t", ":TroubleToggle<cr>", { desc = "Trouble" })
end

M.undotree = function()
	vim.keymap.set("n", "<leader>u", function()
		require("undotree").toggle()
	end, { desc = "Undotree" })
end

return M
