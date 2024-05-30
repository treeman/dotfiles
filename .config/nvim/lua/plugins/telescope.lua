local actions = require("telescope.actions")
local telescope = require("telescope")

telescope.setup({
  defaults = {
    file_ignore_patterns = { "node_modules" },
    mappings = {
      i = {
        ["<esc>"] = actions.close,
      },
    },
    pickers = {
      find_files = {
        hidden = true,
      },
    },
    layout_config = {
      horizontal = {
        preview_width = 0.6,
        width = { padding = 5 },
      },
    },
    path_display = {
      "filename_first",
    },
  },
})

telescope.load_extension("fzf")
