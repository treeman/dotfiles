local config = function()
	local actions = require("telescope.actions")

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
end

return {
	{
		"nvim-telescope/telescope.nvim",
		dependencies = {
			"nvim-lua/popup.nvim",
			"nvim-lua/plenary.nvim",
			"telescope-fzf-native.nvim",
			"telescope-lsp-handlers.nvim",
			"nvim-telescope/telescope-file-browser.nvim",
		},
		config = config,
		cmd = "Telescope",
	},
	{
		"nvim-telescope/telescope-fzf-native.nvim",
		build = "make",
	},
	{
		"gbrlsnchs/telescope-lsp-handlers.nvim",
	},
	{
		"nvim-telescope/telescope-file-browser.nvim",
	},
}