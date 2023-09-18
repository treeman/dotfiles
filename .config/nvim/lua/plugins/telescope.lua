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
end

return {
	{
		"nvim-telescope/telescope.nvim",
		dependencies = {
			"nvim-lua/popup.nvim",
			"nvim-lua/plenary.nvim",
			{
				"nvim-telescope/telescope-fzf-native.nvim",
				build = "make",
			},
			"gbrlsnchs/telescope-lsp-handlers.nvim",
			-- Never really used this
			-- "nvim-telescope/telescope-file-browser.nvim",
			"nvim-treesitter/nvim-treesitter",
		},
		config = config,
		keys = require("config.keymaps").telescope,
		cmd = "Telescope",
		event = { "BufReadPre", "BufNewFile" },
	},
}
