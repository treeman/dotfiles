require("neogit").setup({
  kind = "split_above",
  auto_show_console = false,
  mappings = {
    status = {
      ["="] = "Toggle",
    },
  },
  commit_editor = {
    kind = "auto",
  },
})
