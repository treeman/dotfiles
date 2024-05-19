return {
  "numToStr/Comment.nvim",
  opts = {
    ignore = "^$",
  },
  event = { "BufReadPre", "BufNewFile" },
  dependencies = "nvim-treesitter/nvim-treesitter",
}
