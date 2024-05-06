local opts = {
  highlight = {
    duration = 500,
  },
}

return {
  "kylechui/nvim-surround",
  opts = opts,
  event = "BufReadPre",
  version = "*",
}
