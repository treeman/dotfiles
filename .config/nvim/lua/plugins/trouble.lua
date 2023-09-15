local function config()
	require("trouble").setup({
		padding = false,
		auto_open = false,
		auto_close = false,
	})

	require("config.keymaps").trouble()
end

return {
	"folke/trouble.nvim",
	dependencies = { "nvim-tree/nvim-web-devicons" },
	cmd = { "Trouble", "TroubleToggle", "TroubleClose", "TroubleRefresh" },
	config = config,
}
