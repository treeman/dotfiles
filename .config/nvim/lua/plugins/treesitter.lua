local config = function()
	local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
	parser_config.sdjot = {
		install_info = {
			url = "~/code/tree-sitter-sdjot",
			files = { "src/parser.c", "src/scanner.c" },
			generate_reqires_npm = false,
			requires_generate_from_grammar = false,
		},
		filetype = "sdjot",
	}
	parser_config.djot = {
		install_info = {
			url = "~/code/tree-sitter-djot",
			files = { "src/parser.c", "src/scanner.c" },
			generate_reqires_npm = false,
			requires_generate_from_grammar = false,
		},
		filetype = "djot",
	}

	local keymaps = require("config.keymaps")

	require("nvim-treesitter.configs").setup({
		ensure_installed = {
			"awk",
			"bash",
			"beancount",
			"c",
			"cmake",
			"cpp",
			"css",
			"csv",
			"diff",
			"dockerfile",
			"dot",
			"eex",
			"elixir",
			"fish",
			"git_config",
			"git_rebase",
			"gitattributes",
			"gitcommit",
			"gitignore",
			"gpg",
			"heex",
			"html",
			"http",
			"javascript",
			"jq",
			"json",
			"jsonc",
			"lua",
			"luap",
			"make",
			"markdown",
			"markdown_inline",
			"norg",
			"python",
			"query",
			"regex",
			"ruby",
			"rust",
			"scheme",
			"scss",
			"sql",
			"toml",
			"typescript",
			"vim",
			"vue",
			"xml",
			"yaml",
		},
		highlight = {
			enable = true,
		},
		matchup = {
			enable = true,
		},
		autotag = {
			enable = true,
		},
		endwise = {
			enable = true,
		},
		textobjects = {
			move = {
				enable = true,
				set_jumps = true,
				goto_next_start = keymaps.ts_goto_next_start,
				goto_next_end = keymaps.ts_goto_next_end,
				goto_previous_start = keymaps.ts_goto_previous_start,
				goto_previous_end = keymaps.ts_goto_previous_end,
			},
			swap = {
				enable = true,
				swap_next = keymaps.ts_swap_next,
				swap_previous = keymaps.ts_swap_previous,
			},
			select = {
				enable = true,
				lookahead = true,
				keymaps = keymaps.ts_select,
			},
		},
		playground = {
			enable = true,
			disable = {},
			updatetime = 25,
			persist_queries = true,
			keybindings = {
				toggle_query_editor = "o",
				toggle_hl_groups = "i",
				toggle_injected_languages = "t",
				toggle_anonymous_nodes = "a",
				toggle_language_display = "I",
				focus_language = "f",
				unfocus_language = "F",
				update = "R",
				goto_node = "<cr>",
				show_help = "?",
			},
		},
	})

	-- Make movements repeatable with ; and ,
	local ts_repeat_move = require("nvim-treesitter.textobjects.repeatable_move")
	vim.keymap.set({ "n", "x", "o" }, ";", ts_repeat_move.repeat_last_move)
	vim.keymap.set({ "n", "x", "o" }, ",", ts_repeat_move.repeat_last_move_opposite)
	vim.keymap.set({ "n", "x", "o" }, "f", ts_repeat_move.builtin_f)
	vim.keymap.set({ "n", "x", "o" }, "F", ts_repeat_move.builtin_F)
	vim.keymap.set({ "n", "x", "o" }, "t", ts_repeat_move.builtin_t)
	vim.keymap.set({ "n", "x", "o" }, "T", ts_repeat_move.builtin_T)
end

return {
	{
		"nvim-treesitter/nvim-treesitter",
		config = config,
		build = ":TSUpdate",
		cmd = { "TSInstall", "TSUpdate" },
		event = { "BufReadPre", "BufNewFile" },
		dependencies = {
			"nvim-treesitter/nvim-treesitter-textobjects",
			"nvim-treesitter/nvim-treesitter-context",
			"windwp/nvim-ts-autotag",
			"JoosepAlviste/nvim-ts-context-commentstring",
			"RRethy/nvim-treesitter-endwise",
		},
	},
	{
		"nvim-treesitter/playground",
		cmd = { "TSPlaygroundToggle", "TSHighlightCapturesUnderCursor" },
		dependencies = {
			"nvim-treesitter/nvim-treesitter",
		},
	},
}
