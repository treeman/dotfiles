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
		init = function()
			vim.cmd.colorscheme("melange")
		end,
		config = function()
			-- FIXME doesn't seem possible to do this here, but have to wait until leap is loaded
			-- vim.api.nvim_set_hl(0, "LeapLabelPrimary", { link = "IncSearch" })
		end,
	},
}
