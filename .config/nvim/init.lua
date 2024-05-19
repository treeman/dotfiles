vim.loader.enable()

require("config.leader")

require("config.rocks-nvim")
require("config.options")
require("config.autocmds")
require("config.keymaps").init()
require("config.colorscheme")
require("config.neovide")
require("blog")

require("util.helpers")
