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
