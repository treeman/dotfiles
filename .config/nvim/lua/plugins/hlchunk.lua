return {
  "shellRaining/hlchunk.nvim",
  event = { "BufReadPre", "BufNewFile" },
  config = function()
    require("hlchunk").setup({
      chunk = {
        enable = true,
        use_treesitter = true,
        style = {
          { fg = "#867462" },
          { fg = "#bd8183" },
        },
      },
      indent = {
        enable = true,
        style = "#403a36",
        use_treesitter = true,
        chars = {
          "┆",
        },
      },
    })
  end,
  enabled = false,
}
