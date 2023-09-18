local opts = {
	float_diff = false,
	window = {
		winblend = 0,
	},
}

return {
	"jiaoshijie/undotree",
	opts = opts,
	keys = require("config.keymaps").undotree,
	event = { "BufReadPre", "BufNewFile" },
}
