return {
  "ethanholz/nvim-lastplace",
  opts = {
    lastplace_ignore_buftype = {
      "quickfix",
      "nofile",
      "help",
      "terminal",
    },
    lastplace_ignore_filetype = {
      "dashboard",
      "gitcommit",
      "gitrebase",
      "hgcommit",
      "svn",
      "toggleterm",
    },
    lastplace_open_folds = true,
  },
  lazy = false,
}
