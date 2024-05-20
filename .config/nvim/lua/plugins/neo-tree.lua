require("neo-tree").setup({
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
  filesystem = {
    -- Use oil instead
    hijack_netrw_behavior = "disabled",
  },
})
