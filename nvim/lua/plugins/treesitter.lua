local config = function()
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
				-- Taken keymaps:
				-- [d
				-- goto_next_start = {},
				-- goto_next_end = {},
				-- goto_previous_start = {},
				-- goto_previous_end = {},
			},
		},
		textsubjects = {
			enable = true,
			prev_selection = keymaps.textsubjects.prev_selection,
			keymaps = keymaps.textsubjects.keymaps,
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
end

return {
	{
		"nvim-treesitter/nvim-treesitter",
		config = config,
		lazy = false,
		build = ":TSUpdate",
		dependencies = {
			"nvim-treesitter/nvim-treesitter-textobjects",
			"RRethy/nvim-treesitter-textsubjects",
			"nvim-treesitter/nvim-treesitter-context",
			"windwp/nvim-ts-autotag",
			"JoosepAlviste/nvim-ts-context-commentstring",
			"RRethy/nvim-treesitter-endwise",
		},
	},
	{
		"nvim-treesitter/playground",
		cmd = "TSPlaygroundToggle",
	},
}
