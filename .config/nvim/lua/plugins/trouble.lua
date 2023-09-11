local function config()
	require("trouble").setup({
		padding = false,
		auto_open = true,
		auto_close = true,
	})

	require("config.keymaps").trouble()
end

return {
	"folke/trouble.nvim",
	dependencies = { "nvim-tree/nvim-web-devicons" },
	cmd = { "Trouble", "TroubleToggle", "TroubleClose", "TroubleRefresh" },
	config = config,
}
