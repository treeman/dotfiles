local opts = {
	float_diff = false,
	window = {
		winblend = 0,
	},
}

return {
	"jiaoshijie/undotree",
	config = function()
		require("undotree").setup(opts)
		require("config.keymaps").undotree()
	end,
	event = { "BufReadPre", "BufNewFile" },
}
