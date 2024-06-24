return {
  "chentoast/marks.nvim",
  opts = {
    mappings = require("config.keymaps").marks,
    default_mappings = false,
  },
  event = { "BufReadPost", "BufNewFile" },
}
