require("config.leader")

if vim.g.neovide then
	require("config.neovide")
end

require("config.lazy")

-- Try to set all things after plugins, as some things may possibly be overwritten.
-- `vim.opt.timout` and `ttimeout` had to be set after lazy for instance.
require("config.options")
require("config.colorscheme")
require("config.autocmds")
require("config.keymaps").init()
require("config.commands")
require("blog")
