local opts = {
  signs = {
    add = { text = "+" },
    change = { text = "~" },
    delete = { text = "-" },
    topdelete = { text = "-" },
    changedelete = { text = "~" },
  },
}

return {
  "lewis6991/gitsigns.nvim",
  opts = opts,
  event = "BufReadPre"
}
