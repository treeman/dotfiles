local actions = require("telescope.actions")
local fb_actions = require("telescope._extensions.file_browser.actions")

require("telescope").setup({
	defaults = {
		file_ignore_patterns = { "node_modules" },
		mappings = {
			i = {
				["<esc>"] = actions.close,
			},
		},
		pickers = {
			find_files = {
				hidden = true,
			},
		},
	},
})

-- Makes sorting a lot faster than the built-in one
-- and enables fzf syntax
require("telescope").load_extension("fzf")

require("telescope").load_extension("file_browser")

