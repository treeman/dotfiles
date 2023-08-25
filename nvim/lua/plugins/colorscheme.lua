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
            transparent_mode = true,
            overrides = {
                SignColumn = { link = "dark0" },
                -- ["@variable"] = { fg = "#ea6962" },
                Function = { link = "GruvboxBlue" },
                ErrorMsg = { fg = "#d4be98", bg = "#c14a4a" },
                ["@symbol"] = { link = "GruvboxPurple" },
            },
        }
    }
}
