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
		["core.dirman"] = {
			config = {
				workspaces = {
					vimwiki = "~/vimwiki",
				},
			},
		},
		["core.journal"] = {
			config = { -- Note that this table is optional and doesn't need to be provided
				workspace = "vimwiki",
				strategy = "flat",
			},
		},
		["core.completion"] = {
			config = { -- Note that this table is optional and doesn't need to be provided
				engine = "nvim-cmp",
			},
		},
		["core.concealer"] = {},
	},
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
	overrides = {
		SignColumn = { link = "dark0" },
		-- ["@variable"] = { fg = "#ea6962" },
		Function = { link = "GruvboxBlue" },
		ErrorMsg = { fg = "#d4be98", bg = "#c14a4a" },
		["@symbol"] = { link = "GruvboxPurple" },
	},
})
vim.cmd("colorscheme gruvbox")
