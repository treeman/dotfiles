return {
  "folke/snacks.nvim",
  priority = 1000,
  lazy = false,
  opts = {
    indent = {
      indent = {
        enabled = true,
        char = "┆",
      },
      scope = {
        enabled = true,
        only_current = true,
      },
    },
  },
}
