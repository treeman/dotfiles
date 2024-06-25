local config = function()
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

  -- Makes sorting a lot faster than the built-in one
  -- and enables fzf syntax
  telescope.load_extension("fzf")
end

return {
  {
    "nvim-telescope/telescope.nvim",
    dependencies = {
      "nvim-lua/popup.nvim",
      "nvim-lua/plenary.nvim",
      {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = "make",
      },
      "nvim-treesitter/nvim-treesitter",
      "rachartier/tiny-devicons-auto-colors.nvim",
    },
    config = config,
  },
}
