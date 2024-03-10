-- Keymaps that are sent to plugins during configuration
-- Return a module here so we can keep all keymap definitions
-- in the same place.
-- This is only for global keys, other contexts
-- (such as in the cmp selection menu) may be defined
-- in their respective files.
local M = {}

-- Wrap it in a function to prevent requiring this file evaluates
-- global keymaps multiple times.
M.init = function()
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

	-- Edit file with pre-filled path from the current file
	map("n", "<leader>ef", ":e <C-R>=expand('%:p:h') . '/'<CR>", { desc = "Edit relative file" })

	-- Goto previous buffer
	map("n", "<leader>B", ":edit #<CR>", { desc = "Previous buffer" })

	-- Edit files in a buffer
	map("n", "<leader>ed", ":Oil .<CR>", { desc = "Edit workspace" })
	-- Edit files in within the current directory
	map("n", "<leader>eD", ":Oil <C-R>=expand('%:p:h')<CR><CR>", { desc = "Edit relative workspace" })

	map("n", "<leader>d", ":Neotree toggle=true<CR>", { desc = "Neotree" })
	map("n", "<leader>t", ":TroubleToggle<cr>", { desc = "Trouble" })

	map("n", "<leader>es", ":e ~/norg/scratch.norg<CR>", { desc = "Scratch" })
	map("n", "<leader>ej", require("config.norg").open_weekly_journal, { desc = "This weeks journal" })

	-- Git
	map("n", "gs", ":Neogit<CR>", { desc = "Git status" })
	map("n", "g<space>", ":Git ", { desc = "Git" })
end

M.telescope = {
	{
		"z=",
		function()
			require("telescope.builtin").spell_suggest()
		end,
		silent = true,
		desc = "Spell suggest",
	},
	{ "<leader>f", require("telescope.builtin").find_files, silent = true, desc = "Find files" },
	{
		"<leader>F",
		function()
			require("telescope.builtin").find_files({ cwd = vim.fn.expand("%:p:h") })
		end,
		silent = true,
		desc = "Find relative file",
	},

	{ "<leader>/", require("telescope.builtin").live_grep, silent = true, desc = "Find in files" },
	{ "<leader>b", require("config.telescope_actions").open_buffer, silent = true, desc = "Buffers" },
	{ "<leader>o", require("telescope.builtin").oldfiles, silent = true, desc = "Old files" },

	--  -- I use neorg as a personal knowledge base. Telescoping in it makes it really pleasant,
	{
		"<leader>n",
		function()
			require("config.norg").open_norg("")
		end,
		desc = "Neorg",
	},
	{
		"<leader>ep",
		function()
			require("config.norg").open_norg("projects")
		end,
		desc = "Neorg projects",
	},
	{
		"<leader>ea",
		function()
			require("config.norg").open_norg("areas")
		end,
		desc = "Neorg areas",
	},
	{
		"<leader>er",
		function()
			require("config.norg").open_norg("resources")
		end,
		desc = "Neorg resources",
	},
	{
		"<leader>eA",
		function()
			require("config.norg").open_norg("archive")
		end,
		desc = "Neorg archive",
	},
	-- {
	-- 	"<leader>ej",
	-- 	function()
	-- 		require("config.norg").open_norg("areas/weekly_journal")
	-- 	end,
	-- 	desc = "Neorg archive",
	-- },
	{ "gb", require("telescope.builtin").git_branches, silent = true, desc = "Git branches" },
	-- Ideas
	--require('telescope.builtin').git_commits()
	--require('telescope.builtin').git_bcommits()
	--require('telescope.builtin').git_bcommits_range()
}

M.dial = {
	{
		"<C-a>",
		function()
			require("dial.map").manipulate("increment", "normal")
		end,
		desc = "Increment number",
	},
	{
		"<C-x>",
		function()
			require("dial.map").manipulate("decrement", "normal")
		end,
		desc = "Decrement number",
	},
	{
		"<C-a>",
		mode = { "v" },
		function()
			require("dial.map").manipulate("increment", "visual")
		end,
		desc = "Increment number",
	},
	{
		"<C-x>",
		mode = { "v" },
		function()
			require("dial.map").manipulate("decrement", "visual")
		end,
		desc = "Decrement number",
	},
}

-- Maps four pairs:
-- [f, [F, ]f, ]F
-- for the given treesitter textobject
-- see: https://github.com/nvim-treesitter/nvim-treesitter-textobjects
local ts_move_keys = {
	f = { query = "@function.outer", desc = "goto function" },
	a = { query = "@attribute.inner", desc = "goto attribute" },
	b = { query = "@block.inner", desc = "goto block" },
	c = { query = "@class.outer", desc = "goto class" },
	x = { query = "@comment.outer", desc = "goto comment" },
	g = { query = { "@class.outer", "@function.outer" }, desc = "goto major" },
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

-- Some symbolic keymaps that don't have a string.upper()
M.ts_goto_next_start["]="] = { query = "@statement.outer", desc = "goto statement" }
M.ts_goto_previous_start["[="] = { query = "@statement.outer", desc = "goto statement" }
M.ts_goto_next_start["],"] = { query = "@parameter.outer", desc = "goto parameter" }
M.ts_goto_previous_start["[,"] = { query = "@parameter.outer", desc = "goto parameter" }

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
	["a="] = { query = "@statement.outer", desc = "Select outer statement" },
	["i="] = { query = "@statement.inner", desc = "Select inner statement" },
	["a,"] = { query = "@parameter.outer", desc = "Select outer parameter" },
	["i,"] = { query = "@parameter.inner", desc = "Select inner parameter" },
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

M.trouble = {
	{
		"]t",
		function()
			require("trouble").next({ skip_groups = true, jump = true })
		end,
		desc = "Next trouble",
		silent = true,
	},
	{
		"[t",
		function()
			require("trouble").previous({ skip_groups = true, jump = true })
		end,
		desc = "Prev trouble",
		silent = true,
	},
	{
		"]T",
		function()
			require("trouble").last({ skip_groups = true, jump = true })
		end,
		desc = "Last trouble",
		silent = true,
	},
	{
		"[T",
		function()
			require("trouble").first({ skip_groups = true, jump = true })
		end,
		desc = "First trouble",
		silent = true,
	},
}

M.undotree = {
	{
		"<leader>u",
		function()
			require("undotree").toggle()
		end,
		desc = "Undotree",
	},
}

M.hop = {
	{
		"s",
		function()
			require("hop").hint_char2({})
		end,
		desc = "Sneak",
	},
	{
		"S",
		function()
			require("hop").hint_char2({ direction = require("hop.hint").HintDirection.BEFORE_CURSOR })
		end,
		desc = "Sneak backwards",
	},
	{
		"<leader>w",
		function()
			require("hop").hint_words({})
		end,
		desc = "Hop words",
	},
	{
		"<leader>W",
		function()
			require("hop").hint_words({ direction = require("hop.hint").HintDirection.BEFORE_CURSOR })
		end,
		desc = "Hop words backwards",
	},
}

M.search_replace = {
	-- Replace word under cursor
	{
		"<leader>rw",
		"<cmd>SearchReplaceSingleBufferCWord<cr>",
		desc = "Replace CWord",
	},
	-- Replace WORD under cursor
	{
		"<leader>rW",
		"<cmd>SearchReplaceSingleBufferCWORD<cr>",
		desc = "Replace CWORD",
	},
	-- Replace "expression" (includes dots, not sure how useful this is)
	{
		"<leader>re",
		"<cmd>SearchReplaceSingleBufferCExpr<cr>",
		desc = "Replace CExpr",
	},
	-- Replace visual selection
	{
		"<C-r>",
		mode = "v",
		"<CMD>SearchReplaceSingleBufferVisualSelection<CR>",
		desc = "Replace selection",
	},
}

M.replacer = {
	{
		"<leader>rq",
		function()
			require("replacer").run()
		end,
		silent = true,
		desc = "Make quickfix editable for replacing in",
	},
}

M.pollen = function()
	local map = vim.keymap.set
	map("i", "<C-l>", "λ")
	map("i", "<C-e>", "◊")
end

return M
