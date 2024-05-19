return {
  "nvim-treesitter/nvim-treesitter-context",
  opts = {
    max_lines = 1,
    trim_scope = "inner",
  },
  event = { "BufReadPre", "BufNewFile" },
}
