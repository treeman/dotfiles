return {
  "rachartier/tiny-devicons-auto-colors.nvim",
  dependencies = {
    "nvim-tree/nvim-web-devicons",
  },
  config = function()
    require("tiny-devicons-auto-colors").setup({
      -- Grabbed from melange dark palette
      colors = {
        -- Too dark
        -- "#292522",
        -- "#34302C",
        -- "#403A36",
        -- "#867462",
        "#C1A78E",
        "#ECE1D7",
        "#D47766",
        "#EBC06D",
        "#85B695",
        "#89B3B6",
        "#A3A9CE",
        "#CF9BC2",
        "#BD8183",
        "#E49B5D",
        "#78997A",
        "#7B9695",
        "#7F91B2",
        -- Too dark
        -- "#B380B0",
        -- "#7D2A2F",
        -- "#8B7449",
        -- "#233524",
        -- "#253333",
        -- "#273142",
        -- "#422741",
      },
    })
  end,
}
