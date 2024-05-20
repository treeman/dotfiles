local bg = vim.opt.background:get()
local palette = require("melange/palettes/" .. bg)

-- local a = palette.a -- Grays
-- local b = palette.b -- Bright foreground colors
local c = palette.c -- Foreground colors
-- local d = palette.d -- Background colors

-- See :help neo-tree-highlights
-- See: https://github.com/nvim-treesitter/nvim-treesitter/blob/master/CONTRIBUTING.md#highlights
local overrides = {
  -- Adjust for 0.10
  { name = "FloatBorder", val = { link = "WinSeparator" } },
  -- Fix neo-tree
  { name = "NeoTreeNormal", val = { link = "Normal" } },
  { name = "NeoTreeWinSeparator", val = { link = "WinSeparator" } },
  { name = "NeoTreeTitleBar", val = { link = "Normal" } },
  { name = "NeoTreeFloatTitle", val = { link = "Conceal" } },
  { name = "NeoTreeFloatBorder", val = { link = "WinSeparator" } },
  { name = "NeoTreeFloatNormal", val = { link = "NormalFloat" } },
  { name = "NeoTreeFadeText1", val = { link = "LineNr" } },
  { name = "NeoTreeFadeText2", val = { link = "LineNr" } },
  { name = "NeoTreeDimText", val = { link = "WinSeparator" } },
  -- Fix cmp
  { name = "CmpItemAbbrMatch", val = { link = "Function" } },
  { name = "CmpItemKind", val = { link = "Function" } },
  -- FIXME maybe try to separate these from each other?
  { name = "CmpItemKindText", val = { link = "LineNr" } },
  { name = "CmpItemKindMethod", val = { link = "CursorLineNr" } },
  { name = "CmpItemKindFunction", val = { link = "CursorLineNr" } },
  { name = "CmpItemKindConstructor", val = { link = "CursorLineNr" } },
  { name = "CmpItemKindField", val = { link = "CursorLineNr" } },
  { name = "CmpItemKindVariable", val = { link = "CursorLineNr" } },
  { name = "CmpItemKindClass", val = { link = "CursorLineNr" } },
  { name = "CmpItemKindInterface", val = { link = "CursorLineNr" } },
  { name = "CmpItemKindModule", val = { link = "CursorLineNr" } },
  { name = "CmpItemKindProperty", val = { link = "CursorLineNr" } },
  { name = "CmpItemKindUnit", val = { link = "CursorLineNr" } },
  { name = "CmpItemKindValue", val = { link = "CursorLineNr" } },
  { name = "CmpItemKindEnum", val = { link = "CursorLineNr" } },
  { name = "CmpItemKindKeyword", val = { link = "CursorLineNr" } },
  { name = "CmpItemKindSnippet", val = { link = "CursorLineNr" } },
  { name = "CmpItemKindColor", val = { link = "CursorLineNr" } },
  { name = "CmpItemKindFile", val = { link = "CursorLineNr" } },
  { name = "CmpItemKindReference", val = { link = "CursorLineNr" } },
  { name = "CmpItemKindFolder", val = { link = "CursorLineNr" } },
  { name = "CmpItemKindEnumMember", val = { link = "CursorLineNr" } },
  { name = "CmpItemKindConstant", val = { link = "CursorLineNr" } },
  { name = "CmpItemKindStruct", val = { link = "CursorLineNr" } },
  { name = "CmpItemKindEvent", val = { link = "CursorLineNr" } },
  { name = "CmpItemKindOperator", val = { link = "CursorLineNr" } },
  { name = "CmpItemKindTypeParameter", val = { link = "CursorLineNr" } },
  { name = "CmpItemMenu", val = { link = "Conceal" } },
  -- Marks
  { name = "MarkSignHL", val = { link = "GitSignsChange" } },
  -- Not quite great as MarkSignNumHL overrides current line number coloring
  { name = "MarkSignNumHL", val = { link = "LineNr" } },
  -- Markdown
  { name = "@text.reference.markdown_inline", val = { link = "Type" } },
  { name = "@text.reference.markdown", val = { link = "Type" } },
  -- Djot
  { name = "@markup.heading.1", val = { link = "Title" } },
  { name = "@markup.heading.2", val = { link = "Constant" } },
  { name = "@markup.heading.3", val = { link = "DiagnosticOk" } },
  { name = "@markup.heading.4", val = { link = "DiagnosticInfo" } },
  { name = "@markup.heading.5", val = { link = "DiagnosticHint" } },
  { name = "@markup.heading.6", val = { link = "DiagnosticError" } },
  { name = "@markup.link.url", val = { link = "@string.special.path" } },
  { name = "@markup.highlighted", val = { link = "Special" } },
  { name = "@markup.insert", val = { link = "@markup.underline" } },
  { name = "@markup.superscript", val = { link = "@string" } },
  { name = "@markup.subscript", val = { link = "@string" } },
  { name = "@markup.link.label", val = { link = "@label" } },
  { name = "@markup.link", val = { fg = c.cyan, underline = true } },
  { name = "@none", val = { link = "Normal" } },
  { name = "@markup.math", val = { link = "@markup.italic" } },
  { name = "@markup.strikethrough", val = { link = "@markup.strike" } },
  -- Better elixir colors
  { name = "@symbol.elixir", val = { link = "@label" } },
  { name = "@string.special.symbol.elixir", val = { link = "@label" } },
  { name = "@constant.elixir", val = { link = "Constant" } },
  -- mini
  { name = "MiniTrailSpace", val = { link = "PmenuSel" } },
  -- beancount
  { name = "@property.beancount", val = { link = "Number" } }, -- SEK
  { name = "@field.beancount", val = { link = "Function" } }, -- Dates
  { name = "@constant.beancount", val = { link = "Constant" } }, -- Tags
  -- Random
  { name = "@attribute", val = { link = "Constant" } },
  { name = "LeapLabelPrimary", val = { link = "IncSearch" } },
}

for _, v in pairs(overrides) do
  vim.api.nvim_set_hl(0, v.name, v.val)
end
