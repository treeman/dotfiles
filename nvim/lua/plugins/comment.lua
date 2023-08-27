return {
  'numToStr/Comment.nvim',
  opts = {
    ignore = "^$",
  },
  event = "BufReadPre",
  dependencies = "nvim-treesitter/nvim-treesitter",
}
