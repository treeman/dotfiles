local config = function()
	local cmp = require("cmp")
	local context = require("cmp.config.context")
	local lspkind = require("lspkind")

	cmp.setup({
		enabled = true,
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
			{
				name = "luasnip",
				group_index = 1,
				entry_filter = function()
					return not context.in_treesitter_capture("string") and not context.in_syntax_group("String")
				end,
			},
			{
				name = "nvim_lsp",
				group_index = 2,
			},
			{
				name = "nvim_lua",
				group_index = 2,
			},
			{
				name = "neorg",
				group_index = 2,
			},
			{
				name = "beancount",
				option = {
					account = "/home/tree/vimwiki/money/accounting/personal.beancount",
				},
				group_index = 2,
			},
			{
				name = "spell",
				option = {
					enable_in_context = function()
						return context.in_treesitter_capture("spell")
					end,
				},
				group_index = 3,
			},
			{
				name = "treesitter",
				keyword_length = 3,
				group_index = 3,
			},
			{
				name = "async_path",
				group_index = 3,
			},
			{
				name = "buffer",
				keyword_length = 3,
				group_index = 4,
			},
			{
				name = "calc",
				group_index = 4,
			},
		},
		formatting = {
			format = lspkind.cmp_format({
				mode = "symbol_text",
				ellipsis_char = "…",
				menu = {
					luasnip = "[SNIP]",
					nvim_lsp = "[LSP]",
					nvim_lua = "[LUA]",
					neorg = "[NORG]",
					beancount = "[BEAN]",
					spell = "[SPELL]",
					treesitter = "[TREE]",
					async_path = "[PATH]",
					buffer = "[BUF]",
					calc = "[CALC]",
				},
			}),
		},
		sorting = {
			priority_weight = 2,
			comparators = {
				cmp.config.compare.offset,
				cmp.config.compare.exact,
				cmp.config.compare.score,
				cmp.config.compare.recently_used,
				cmp.config.compare.locality,
				cmp.config.compare.kind,
				cmp.config.compare.sort_text,
				cmp.config.compare.length,
				cmp.config.compare.order,
			},
		},
	})

	cmp.setup.cmdline("/", {
		sources = {
			{ name = "buffer" },
		},
	})
end

return {
	{
		"hrsh7th/nvim-cmp",
		config = config,
		dependencies = {
			"FelipeLema/cmp-async-path",
			"crispgm/cmp-beancount",
			"f3fora/cmp-spell",
			"hrsh7th/cmp-buffer",
			"hrsh7th/cmp-calc",
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-nvim-lua",
			"ray-x/cmp-treesitter",
			"saadparwaiz1/cmp_luasnip",
			"onsails/lspkind.nvim",
		},
		event = "InsertEnter",
	},
}
