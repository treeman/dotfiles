return {
  "https://github.com/nvim-pack/nvim-spectre",
  event = { "BufReadPre", "BufNewFile" },
  cmd = "Spectre",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "folke/trouble.nvim",
  },
}
