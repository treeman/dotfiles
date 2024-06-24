vim.loader.enable()

require("config.leader")
require("config.lazy")
require("config.options")
require("config.autocmds")
require("config.keymaps").init()
require("config.colorscheme")
require("config.neovide")
require("blog")

require("util.helpers")

-- Easy print function...
P = function(v)
  print(vim.inspect(v))
  return v
end
