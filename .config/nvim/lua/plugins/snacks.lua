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
    scroll = {
      animate = {
        duration = { step = 15, total = 150 },
      },
    },
    -- This highlights words using LSP instead of the other plugin I have that highlights text.
    -- words = {},
  },
}
