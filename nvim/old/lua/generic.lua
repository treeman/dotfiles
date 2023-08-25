require("gitsigns").setup({
	signs = {
		add = { text = "+" },
		change = { text = "~" },
		delete = { text = "-" },
		topdelete = { text = "-" },
		changedelete = { text = "~" },
	},
})

-- Setup nvim-cmp.
local cmp = require("cmp")

cmp.setup({
	snippet = {
		expand = function(args)
			vim.fn["vsnip#anonymous"](args.body)
		end,
	},
	mapping = {
		["<C-n>"] = cmp.mapping(cmp.mapping.select_next_item(), { "i", "c" }),
		["<C-p>"] = cmp.mapping(cmp.mapping.select_prev_item(), { "i", "c" }),
		["<PgUp>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" }),
		["<PgDn>"] = cmp.mapping(cmp.mapping.scroll_docs(4), { "i", "c" }),
		["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
		["<C-e>"] = cmp.mapping({
			i = cmp.mapping.abort(),
			c = cmp.mapping.close(),
		}),
		["<Tab>"] = cmp.mapping.confirm({ select = true }),
	},
	sources = {
		{ name = "nvim_lsp" },
		{ name = "vsnip" },
		{ name = "buffer" },
		{ name = "neorg" },
		{
			name = "spell",
			option = {
				enable_in_context = function()
					return require("cmp.config.context").in_treesitter_capture("spell")
				end,
			},
		},
		{ name = "calc" },
		{ name = "path" },
		{
			name = "beancount",
			option = {
				account = "/home/tree/vimwiki/money/accounting/personal.beancount",
			},
		},
	},
})

vim.opt.spell = true
vim.opt.spelllang = { "en_us" }

-- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline("/", {
	sources = {
		{ name = "buffer" },
	},
})

require("cheatsheet").setup({
	bundled_cheatsheets = false,
	bundled_plugin_cheatsheets = false,
})

require("indent_blankline").setup({
	use_treesitter = true,
	use_treesitter_scope = true,
	space_char_blankline = " ",
	show_current_context = true,
	show_trailing_blankline_indent = false,
	-- Maybe could be ok...
	--show_current_context_start = true,
})

require("neorg").setup({
	load = {
		["core.defaults"] = {},
		["core.completion"] = { config = { engine = "nvim-cmp" } },
		["core.integrations.nvim-cmp"] = {},
		["core.dirman"] = {
			config = {
				workspaces = {
					norg = "~/norg",
					projects = "~/norg/projects",
					areas = "~/norg/areas",
					resources = "~/norg/resources",
				},
			},
		},
		-- ["core.journal"] = { config = { workspace = "norg", strategy = "flat" } },
		["core.concealer"] = {
			config = {
				icons = {
					todo = {
						done = { icon = "✓" },
						pending = { icon = "▶" },
						uncertain = { icon = "⁇" },
						on_hold = { icon = "⏸" },
						cancelled = { icon = "⏏" },
						undone = { icon = " " },
					},
				},
			},
		},
	},
})
require("trouble").setup({
	auto_jump = { "lsp_definitions", "lsp_references" },
	-- We really should make icons work...
	icons = false,
	fold_open = "v", -- icon used for open folds
	fold_closed = ">", -- icon used for closed folds
	indent_lines = false, -- add an indent guide below the fold icons
	signs = {
		-- icons / text used for a diagnostic
		error = "error",
		warning = "warn",
		hint = "hint",
		information = "info",
	},
	use_diagnostic_signs = false,
})

require("gruvbox").setup({
	contrast = "hard", -- can be "hard", "soft" or empty string
	palette_overrides = {
		-- See colors:
		-- https://github.com/ellisonleao/gruvbox.nvim/blob/main/lua/gruvbox/palette.lua

		-- From
		-- https://github.com/eddyekofo94/gruvbox-flat.nvim/blob/master/lua/gruvbox/colors.lua
		light1 = "#d4be98",
		gray = "#7c6f64",
		bright_blue = "#7daea3",
		bright_aqua = "#89b482",
		bright_purple = "#d3869b",
		bright_orange = "#e78a4e",
		bright_yellow = "#d8a657",
		neutral_yellow = "#b47109",
		bright_green = "#a9b665",
		bright_red = "#ea6962",
		neutral_red = "#c14a4a",
	},
	transparent_mode = true,
	overrides = {
		SignColumn = { link = "dark0" },
		-- ["@variable"] = { fg = "#ea6962" },
		Function = { link = "GruvboxBlue" },
		ErrorMsg = { fg = "#d4be98", bg = "#c14a4a" },
		["@symbol"] = { link = "GruvboxPurple" },
	},
})
require("kanagawa").setup({
	statementStyle = { bold = false },
	transparent = false,
	colors = {
		palette = {},
		theme = {
			all = {
				ui = {
					bg_gutter = "none",
				},
			},
		},
	},
	overrides = function(colors) -- add/modify highlights
		return {}
	end,
	theme = "wave", -- Load "wave" theme when 'background' option is not set
	background = { -- map the value of 'background' option to a theme
		dark = "wave", -- try "dragon" !
		light = "lotus",
	},
})

vim.cmd("colorscheme gruvbox")
-- vim.cmd("colorscheme kanagawa")
