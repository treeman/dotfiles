require("oil").setup({
  default_file_explorer = true,
  keymaps = {
    ["<BS>"] = "actions.parent",
    ["!"] = "actions.toggle_hidden",
  },
})
