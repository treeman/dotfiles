local opts = {
	-- T-34 adjusted comfortable keys
	keys = "eatnhisldroyucwfgv",
}

return {
	"smoka7/hop.nvim",
	version = "*",
	opts = opts,
	keys = require("config.keymaps").hop,
	enabled = false,
}
