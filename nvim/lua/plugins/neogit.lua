local opts = {
	kind = "split_above",
	auto_show_console = false,
	mappings = {
		status = {
			["="] = "Toggle",
		},
	},
	commit_editor = {
		kind = "auto",
	},
}

return {
	"NeogitOrg/neogit",
	dependencies = {
		"nvim-lua/plenary.nvim",
		"nvim-telescope/telescope.nvim",
		"sindrets/diffview.nvim",
	},
	opts = opts,
	cmd = "Neogit",
}
