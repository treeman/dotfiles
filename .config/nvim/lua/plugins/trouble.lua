opts = {
	padding = false,
	auto_open = false,
	auto_close = false,
}

return {
	"folke/trouble.nvim",
	dependencies = { "nvim-tree/nvim-web-devicons" },
	cmd = { "Trouble", "TroubleToggle", "TroubleClose", "TroubleRefresh" },
	opts = opts,
	keys = require("config.keymaps").trouble,
}
