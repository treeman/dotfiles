local opts = {
	-- T-34 adjusted comfortable keys
	labels = "eatnhisldroyucwfgv",
	label = {
		uppercase = false,
		current = false, -- don't add a label for the first match, use <CR> instead
	},
	modes = {
		char = {
			-- Maybe not so super nice?
			jump_labels = false,
		},
	},
}

return {
	-- "folke/flash.nvim",
	-- event = { "BufReadPre", "BufNewFile" },
	-- opts = opts,
	-- keys = require("config.keymaps").flash,
}
