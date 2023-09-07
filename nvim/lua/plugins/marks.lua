return {
	"chentoast/marks.nvim",
	config = function()
		require("marks").setup({
			builtin_marks = { ".", "<", ">", "^" },
			mappings = require("config.keymaps").marks,
			default_mappings = false,
		})
		-- Not quite great as MarkSignNum overrides current line number coloring
		vim.api.nvim_set_hl(0, "MarkSignHL", { link = "GitSignsChange" })
		vim.api.nvim_set_hl(0, "MarkSignNumHL", { link = "LineNr" })
	end,
	event = "BufReadPre",
}
