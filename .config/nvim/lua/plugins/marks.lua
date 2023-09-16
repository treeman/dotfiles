return {
	"chentoast/marks.nvim",
	config = function()
		require("marks").setup({
			builtin_marks = { ".", "<", ">", "'" },
			mappings = require("config.keymaps").marks,
			default_mappings = false,
		})
	end,
	event = { "BufReadPre", "BufNewFile" },
}
