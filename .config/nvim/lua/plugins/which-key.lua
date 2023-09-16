return {
	"folke/which-key.nvim",
	event = { "BufReadPre", "BufNewFile" },
	keys = { "<leader>" },
	opts = {
		plugins = {
			marks = true,
			registers = true,
			presets = {
				operators = true,
				motions = true,
				text_objects = true,
				windows = true,
				z = true,
				g = true,
			},
		},
	},
}
