return {
	{
		"catppuccin/nvim",
		name = "catppuccin",
		config = function()
			require("catppuccin").setup({
				flavor = "mocha",
			})
		end,
	},
	{ "EdenEast/nightfox.nvim" },
	{ "sainnhe/everforest" },
	{
		"savq/melange-nvim",
		priority = 1000,
	},
}
