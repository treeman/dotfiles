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

	{ name = "@text.reference.markdown_inline", val = { link = "Type" } },
	{ name = "@text.reference.markdown", val = { link = "Type" } },
}

for _, v in pairs(overrides) do
	vim.api.nvim_set_hl(0, v.name, v.val)
end
