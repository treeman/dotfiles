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

-- require("gruvbox").setup({
-- 	contrast = "hard", -- can be "hard", "soft" or empty string
-- 	-- Stolen from https://www.reddit.com/r/neovim/comments/10znmr2/whats_your_neovim_colorscheme_in_2023/j8504j1/
-- 	-- dunno if we should keep this
-- 	overrides = {
-- 		Normal = { bg = "#0E1018" },
-- 		VertSplit = { bg = "#0E1018" },
-- 		SignColumn = { bg = "#ff9900" },
-- 		Define = { link = "GruvboxPurple" },
-- 		Macro = { link = "GruvboxPurple" },
-- 		["@constant.builtin"] = { link = "GruvboxPurple" },
-- 		["@storageclass.lifetime"] = { link = "GruvboxAqua" },
-- 		["@text.note"] = { link = "TODO" },
-- 		CocCodeLens = { fg = "#878787" },
-- 		ContextVt = { fg = "#878787" },
-- 		Comment = { fg = "#fe8019", italic = true },
-- 		Folded = { italic = true, fg = "#fe8019", bg = "#3c3836" },
-- 		FoldColumn = { fg = "#fe8019", bg = "#0E1018" },
-- 		DiffAdd = { bold = true, reverse = false, fg = "", bg = "#2a4333" },
-- 		DiffChange = { bold = true, reverse = false, fg = "", bg = "#333841" },
-- 		DiffDelete = { bold = true, reverse = false, fg = "#442d30", bg = "#442d30" },
-- 		DiffText = { bold = true, reverse = false, fg = "", bg = "#213352" },
-- 		StatusLine = { bg = "#ffffff", fg = "#0E1018" },
-- 		StatusLineNC = { bg = "#3c3836", fg = "#0E1018" },
-- 		CursorLineNr = { fg = "#fabd2f", bg = "#0E1018" },
-- 		CocWarningFloat = { fg = "#dfaf87" },
-- 		CocInlayHint = { fg = "#87afaf" },
-- 		DiagnosticVirtualTextWarn = { fg = "#dfaf87" },
-- 		GruvboxOrangeSign = { fg = "#dfaf87", bg = "#0E1018" },
-- 		GruvboxAquaSign = { fg = "#8EC07C", bg = "#0E1018" },
-- 		GruvboxGreenSign = { fg = "#b8bb26", bg = "#0E1018" },
-- 		GruvboxRedSign = { fg = "#fb4934", bg = "#0E1018" },
-- 		GruvboxBlueSign = { fg = "#83a598", bg = "#0E1018" },
-- 		WilderMenu = { fg = "#ebdbb2", bg = "#0E1018" },
-- 		WilderAccent = { fg = "#f4468f", bg = "#0E1018" },
-- 	},
-- })
-- vim.cmd("colorscheme gruvbox")
