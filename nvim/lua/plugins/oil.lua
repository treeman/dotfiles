local opts = {
  default_file_explorer = true,
  keymaps = {
    ["<BS>"] = "actions.parent",
    ["!"] = "actions.toggle_hidden",
  }
}

return {
  "stevearc/oil.nvim",
  -- Not sure how to lazy load this so it can capture "nvim ."
  lazy = false,
  dependencies = { "nvim-tree/nvim-web-devicons" },
  opts = opts,
}
