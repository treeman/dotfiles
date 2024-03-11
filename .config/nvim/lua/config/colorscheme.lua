-- vim.cmd.colorscheme("kanagawa")
vim.cmd.colorscheme("melange")

-- See :help neo-tree-highlights
-- See: https://github.com/nvim-treesitter/nvim-treesitter/blob/master/CONTRIBUTING.md#highlights
local overrides = {
	{ name = "NeoTreeNormal", val = { link = "Normal" } },
	{ name = "NeoTreeWinSeparator", val = { link = "WinSeparator" } },
	{ name = "LeapLabelPrimary", val = { link = "IncSearch" } },
	{ name = "MarkSignHL", val = { link = "GitSignsChange" } },
	-- Not quite great as MarkSignNumHL overrides current line number coloring
	{ name = "MarkSignNumHL", val = { link = "LineNr" } },
	-- Attempt to make Neorg more beautiful...
	{ name = "@neorg.headings.3.prefix.norg", val = { link = "Number" } },
	{ name = "@neorg.headings.3.title.norg", val = { link = "Number" } },
	{ name = "@neorg.headings.6.prefix.norg", val = { link = "Constant" } },
	{ name = "@neorg.headings.6.title.norg", val = { link = "Constant" } },
	{ name = "@attribute", val = { link = "Constant" } },
	{ name = "@neorg.markup.inline_math.norg", val = { link = "Statement" } },
	{ name = "@neorg.markup.spoiler.norg", val = { link = "Operator" } },
	{ name = "@neorg.markup.variable.norg", val = { link = "@string.regex" } },
	-- Markdown
	{ name = "@text.reference.markdown_inline", val = { link = "Type" } },
	{ name = "@text.reference.markdown", val = { link = "Type" } },
	-- Djot
	{ name = "@markup.highlighted", val = { link = "Special" } },
	{ name = "@markup.insert", val = { link = "@markup.underline" } },
	{ name = "@markup.delete", val = { link = "@markup.strike" } },
	{ name = "@markup.superscript", val = { link = "@string" } },
	{ name = "@markup.subscript", val = { link = "@string" } },
	{ name = "@markup.symbol", val = { link = "@string.special" } },
	{ name = "@markup.math", val = { link = "@markup.italic" } },
	{ name = "@markup.caption", val = { link = "@markup.italic" } },
	{ name = "@markup.link.label", val = { link = "@label" } },
	{ name = "@markup.link.reference", val = { link = "Constant" } },
	{ name = "@markup.link.definition", val = { link = "Constant" } },
	{ name = "@markup.link.url", val = { link = "@string.special.path" } },
	{ name = "@markup.footnote.definition", val = { link = "Function" } },
	{ name = "@markup.footnote.reference", val = { link = "Function" } },
	{ name = "@markup.todo", val = { link = "Todo" } },
	{ name = "@markup.note", val = { link = "Todo" } },
	{ name = "@markup.fixme", val = { link = "Todo" } },
	-- Better elixir colors
	{ name = "@symbol.elixir", val = { link = "@label" } },
	{ name = "@constant.elixir", val = { link = "Constant" } },
	-- mini
	{ name = "MiniTrailSpace", val = { link = "PmenuSel" } },
	-- beancount
	{ name = "@property.beancount", val = { link = "Number" } }, -- SEK
	{ name = "@field.beancount", val = { link = "Function" } }, -- Dates
	{ name = "@constant.beancount", val = { link = "Constant" } }, -- Tags
}

for _, v in pairs(overrides) do
	vim.api.nvim_set_hl(0, v.name, v.val)
end
