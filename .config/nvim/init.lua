vim.loader.enable()

require("config.leader")
require("config.lazy")
require("config.options")
require("config.autocmds")
require("config.usercmds")
require("config.keymaps").init()
require("config.colorscheme")
require("config.neovide")
require("blog")

require("util.helpers")

-- Easy print function...
P = function(...)
  print(vim.inspect(...))
  return ...
end
