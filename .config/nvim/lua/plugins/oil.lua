return {
  "stevearc/oil.nvim",
  opts = {
    default_file_explorer = true,
    keymaps = {
      ["<BS>"] = "actions.parent",
      ["!"] = "actions.toggle_hidden",
    },
  },
  lazy = false,
}
