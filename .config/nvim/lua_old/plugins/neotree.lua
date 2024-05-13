-- If this doesn't work well, look at nvim-tree

local opts = {
  -- use_default_mappings = false,
  window = {
    mapping_options = {
      noremap = true,
      nowait = true,
    },
    mappings = {
      ["l"] = "open",
      ["h"] = "close_node",
      ["!"] = "toggle_hidden",
      ["d"] = "add_directory",
      ["X"] = "delete",
      ["/"] = "noop", -- Disable fuzzy finder thx
      ["?"] = "noop", -- Disable fuzzy finder thx
      ["space"] = "none",
      ["<Esc>"] = "close_window",
    },
  },
}

return {
  "nvim-neo-tree/neo-tree.nvim",
  branch = "v3.x",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-tree/nvim-web-devicons",
    "MunifTanjim/nui.nvim",
    "s1n7ax/nvim-window-picker",
  },
  cmd = "Neotree",
  opts = opts,
}
