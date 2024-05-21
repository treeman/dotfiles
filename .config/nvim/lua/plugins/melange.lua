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
  -- { name = "CmpItemKind", val = { link = "Function" } },
  { name = "CmpItemKindText", val = { link = "Conceal" } },
  { name = "CmpItemKindMethod", val = { link = "@lsp.type.meThod" } },
  { name = "CmpItemKindFunction", val = { link = "@lsp.type.function" } },
  { name = "CmpItemKindConstructor", val = { link = "@lsp.type.function" } },
  { name = "CmpItemKindField", val = { link = "Identifier" } },
  { name = "CmpItemKindVariable", val = { link = "@lsp.type.variable" } },
  { name = "CmpItemKindClass", val = { link = "@lsp.type.class" } },
  { name = "CmpItemKindInterface", val = { link = "@lsp.type.class" } },
  { name = "CmpItemKindModule", val = { link = "@lsp.type.namespace" } },
  { name = "CmpItemKindProperty", val = { link = "@lsp.type.property" } },
  { name = "CmpItemKindUnit", val = { link = "Number" } },
  { name = "CmpItemKindValue", val = { link = "Number" } },
  { name = "CmpItemKindEnum", val = { link = "@lsp.type.enum" } },
  { name = "CmpItemKindKeyword", val = { link = "@lsp.type.keyword" } },
  { name = "CmpItemKindSnippet", val = { link = "Number" } },
  { name = "CmpItemKindColor", val = { link = "Operator" } },
  { name = "CmpItemKindFile", val = { link = "Statement" } },
  { name = "CmpItemKindReference", val = { link = "CursorLineNr" } },
  { name = "CmpItemKindFolder", val = { link = "Special" } },
  { name = "CmpItemKindEnumMember", val = { link = "@lsp.type.enumMember" } },
  { name = "CmpItemKindConstant", val = { link = "Constant" } },
  { name = "CmpItemKindStruct", val = { link = "@lsp.type.struct" } },
  { name = "CmpItemKindEvent", val = { link = "Special" } },
  { name = "CmpItemKindOperator", val = { link = "@lsp.type.operator" } },
  { name = "CmpItemKindTypeParameter", val = { link = "@lsp.type.typeParameter" } },
  { name = "CmpItemMenu", val = { link = "Conceal" } },

  -- New lsp semantic tokens `:help lsp-semantic-highlight`
  -- { name = "@lsp.mod.abstract", val = { link = "" } }, --        Types and member functions that are abstract
  -- { name = "@lsp.mod.async", val = { link = "Statement" } }, --           Functions that are marked async
  -- { name = "@lsp.mod.declaration", val = { link = "" } }, --     Declarations of symbols
  -- { name = "@lsp.mod.defaultLibrary", val = { link = "" } }, --  Symbols that are part of the standard library
  -- { name = "@lsp.mod.definition", val = { link = "" } }, --      Definitions of symbols, for example, in header files
  -- { name = "@lsp.mod.deprecated", val = { link = "" } }, --      Symbols that should no longer be used
  { name = "@lsp.mod.documentation", val = { link = "@label" } }, --   Occurrences of symbols in documentation
  -- { name = "@lsp.mod.modification", val = { link = "" } }, --    Variable references where the variable is assigned to
  { name = "@lsp.mod.readonly", val = { link = "Constant" } }, --        Readonly variables and member fields (constants)
  -- { name = "@lsp.mod.static", val = { link = "Statement" } },

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
