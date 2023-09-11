local opts = {
	load = {
		["core.defaults"] = {},
		["core.completion"] = { config = { engine = "nvim-cmp" } },
		["core.integrations.nvim-cmp"] = {},
		["core.dirman"] = {
			config = {
				workspaces = {
					norg = "~/norg",
					projects = "~/norg/projects",
					areas = "~/norg/areas",
					resources = "~/norg/resources",
					archive = "~/norg/archive",
				},
			},
		},
		-- ["core.journal"] = { config = { workspace = "norg", strategy = "flat" } },
		["core.concealer"] = {
			config = {
				icons = {
					todo = {
						done = { icon = "✓" },
						pending = { icon = "▶" },
						uncertain = { icon = "⁇" },
						on_hold = { icon = "⏸" },
						cancelled = { icon = "⏏" },
						undone = { icon = " " },
					},
				},
			},
		},
	},
}

return {
	{
		"nvim-neorg/neorg",
		build = ":Neorg sync-parsers",
		dependencies = { "nvim-lua/plenary.nvim" },
		ft = "norg",
		opts = opts,
	},
}
