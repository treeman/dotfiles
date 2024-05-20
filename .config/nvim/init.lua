vim.loader.enable()

require("config.leader")

require("config.rocks-nvim")
require("config.options")
require("config.autocmds")
require("config.keymaps").init()
require("config.colorscheme")
require("config.neovide")
require("config.lazy")
require("blog")

require("util.helpers")
