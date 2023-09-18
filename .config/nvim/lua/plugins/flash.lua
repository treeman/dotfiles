local opts = {
	-- T-34 adjusted comfortable keys
	labels = "eatnhisldroyucwfgv",
	-- jump = {
	-- autojump = true,
	-- },
	label = {
		uppercase = false,
	},
	modes = {
		char = {
			-- FIXME for some reason these don't show up?
			jump_labels = true,
		},
	},
}

return {
	"folke/flash.nvim",
	event = { "BufReadPre", "BufNewFile" },
	opts = opts,
	keys = require("config.keymaps").flash,
}
