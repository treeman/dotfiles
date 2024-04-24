local function in_comment()
	local context = require("cmp.config.context")
	return context.in_treesitter_capture("comment") or context.in_syntax_group("Comment")
end

local function in_string()
	local context = require("cmp.config.context")
	return context.in_treesitter_capture("string") or context.in_syntax_group("String")
end

local function in_spell()
	local context = require("cmp.config.context")
	return context.in_treesitter_capture("spell")
end

local function disallowed_buftype()
	local buftype = vim.bo.buftype
	local disallowed = {
		"prompt",
		"nofile",
	}
	for _, v in pairs(disallowed) do
		if buftype == v then
			return true
		end
	end
	return false
end

local blog_type_rank = {
	BrokenLink = 0,
	LinkDef = 1,
	Heading = 2,
	Post = 3,
	Tag = 4,
	Series = 5,
	Standalone = 6,
	Constant = 7,
	Img = 8,
	DivClass = 9,
}

local function blog_compare(entry1, entry2)
	-- Only sort blog entries.
	if entry1.source.name ~= "blog" or entry2.source.name ~= "blog" then
		return nil
	end

	local info1 = entry1.completion_item.info
	local info2 = entry2.completion_item.info

	local rank1 = blog_type_rank[info1.type]
	local rank2 = blog_type_rank[info2.type]
	if rank1 < rank2 then
		return true
	elseif rank1 > rank2 then
		return false
	end

	if info1.type == "Img" then
		if info1.modified > info2.modified then
			return true
		else
			return false
		end
	elseif info1.type == "Post" then
		if info1.created > info2.created then
			return true
		else
			return false
		end
	elseif info1.type == "Series" then
		if info1.posts[1].created > info2.posts[1].created then
			return true
		else
			return false
		end
	elseif info1.type == "Tag" then
		if #info1.posts > #info2.posts then
			return true
		else
			return false
		end
	end

	return nil
end

local function config()
	local cmp = require("cmp")
	local lspkind = require("lspkind")
	local luasnip = require("luasnip")

	cmp.setup({
		enabled = function()
			return not in_comment() and not disallowed_buftype()
		end,
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
		snippet = {
			expand = function(args)
				luasnip.lsp_expand(args.body)
			end,
		},
		sources = {
			{
				name = "luasnip",
				group_index = 1,
				entry_filter = function()
					return not in_string()
				end,
			},
			{
				name = "blog",
				group_index = 1,
			},
			{
				name = "nvim_lsp",
				group_index = 2,
			},
			{
				name = "nvim_lua",
				group_index = 3,
			},
			{
				name = "neorg",
				group_index = 3,
			},
			-- {
			-- 	name = "beancount",
			-- 	option = {
			-- 		account = "/home/tree/vimwiki/money/accounting/personal.beancount",
			-- 	},
			-- 	group_index = 3,
			-- },
			{
				name = "spell",
				option = {
					enable_in_context = in_spell,
				},
				group_index = 3,
			},
			{
				name = "treesitter",
				keyword_length = 3,
				group_index = 4,
			},
			{
				name = "async_path",
				group_index = 4,
			},
			{
				name = "calc",
				group_index = 4,
			},
			{
				name = "buffer",
				keyword_length = 3,
				group_index = 5,
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
					blog = "[BLOG]",
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
				blog_compare,
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
			"L3MON4D3/LuaSnip",
			"crispgm/cmp-beancount",
			"f3fora/cmp-spell",
			"hrsh7th/cmp-buffer",
			"hrsh7th/cmp-calc",
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-nvim-lua",
			"onsails/lspkind.nvim",
			"nvim-treesitter/nvim-treesitter",
			"ray-x/cmp-treesitter",
			"saadparwaiz1/cmp_luasnip",
		},
		event = "InsertEnter",
	},
}
