local opts = {
  disabled_keys = {
    -- up/down is aliased to gj and gk, and besides using the T-34 keyboard layout arrows aren't a problem.
    ["<Up>"] = {},
    ["<Down>"] = {},
    ["<Left>"] = {},
    ["<Right>"] = {},
  },
  disabled_filetypes = {
    "qf",
    "NvimTree",
    "lazy",
    "mason",
    "oil",
    "alpha",
    "neo-tree",
    "NeogitStatus",
  },
  allow_different_key = true,
  disable_mouse = false,
}

return {
  "m4xshen/hardtime.nvim",
  dependencies = { "MunifTanjim/nui.nvim", "nvim-lua/plenary.nvim" },
  opts = opts,
  event = { "BufReadPre", "BufNewFile" },
}
