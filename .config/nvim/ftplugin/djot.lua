vim.opt_local.foldmethod = "expr"
vim.opt_local.foldexpr = "nvim_treesitter#foldexpr()"
vim.bo.commentstring = "{%%s%}"

require("config.keymaps").djot()
