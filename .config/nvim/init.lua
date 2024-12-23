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
require("org")

require("util.helpers")

-- Easy print function...
P = function(...)
  print(vim.inspect(...))
  return ...
end

-- TODO we should use this during development for keymaps etc
R = function(pkg)
  package.loaded[pkg] = nil
  return require(pkg)
end
