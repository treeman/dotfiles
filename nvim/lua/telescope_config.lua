local actions = require("telescope.actions")
require("telescope").setup({
	defaults = {
		file_ignore_patterns = { "node_modules" },
		mappings = {
			i = {
				["<esc>"] = actions.close,
			},
		},
	},
})

-- Makes sorting a lot faster than the built-in one
-- and enables fzf syntax
require("telescope").load_extension("fzf")
