local light1 = "#d4be98"
local gray = "#7c6f64"
local bright_blue = "#7daea3"
local bright_aqua = "#89b482"
local bright_purple = "#d3869b"
local bright_orange = "#e78a4e"
local bright_yellow = "#d8a657"
local neutral_yellow = "#b47109"
local bright_green = "#a9b665"
local bright_red = "#ea6962"
local neutral_red = "#c14a4a"

return {
  {
    "ellisonleao/gruvbox.nvim",
    priority = 1000,
    opts = {
      contrast = "hard",
      palette_overrides = {
        -- See colors:
        -- https://github.com/ellisonleao/gruvbox.nvim/blob/main/lua/gruvbox/palette.lua

        -- From
        -- https://github.com/eddyekofo94/gruvbox-flat.nvim/blob/master/lua/gruvbox/colors.lua
        light1 = light1,
        gray = gray,
        bright_blue = bright_blue,
        bright_aqua = bright_aqua,
        bright_purple = bright_purple,
        bright_orange = bright_orange,
        bright_yellow = bright_yellow,
        neutral_yellow = neutral_yellow,
        bright_green = bright_green,
        bright_red = bright_red,
        neutral_red = neutral_red,
      },
      transparent_mode = true,
      overrides = {
        SignColumn = { link = "dark0" },
        -- ["@variable"] = { fg = "#ea6962" },
        Function = { link = "GruvboxBlue" },
        ErrorMsg = { fg = light1, bg = neutral_red },
        ["@symbol"] = { link = "GruvboxPurple" },
        -- Not sure what to do about these
        LeapLabelPrimary = { link = "IncSearch" },

        -- FIXME should do these properly. Diff view always sucks
        -- NeogitHunkHeader
        NeogitDiffContext = { link = "Text" },
        NeogitDiffAdd = { fg = bright_green },
        NeogitDiffDelete = { fg = bright_red },
        NeogitDiffHeader = { link = "NonText" },
        -- line highlights current context
        -- NeogitHunkHeaderHighlight
        NeogitDiffContextHighlight = { link = "NeogitDiffContext" },
        NeogitDiffAddHighlight = { link = "NeogitDiffAdd" },
        NeogitDiffDeleteHighlight = { link = "NeogitDiffDelete" },
        NeogitDiffHeaderHighlight = { link = "NeogitDiffHeaderHighlight" },

        --Existing things in gruvbox
        -- DiffDelete = { fg = colors.red, bg = colors.bg0, reverse = config.inverse },
        -- DiffAdd = { fg = colors.green, bg = colors.bg0, reverse = config.inverse },
        -- DiffChange = { fg = colors.aqua, bg = colors.bg0, reverse = config.inverse },
        -- DiffText = { fg = colors.yellow, bg = colors.bg0, reverse = config.inverse },
      },
    },
  },
}
