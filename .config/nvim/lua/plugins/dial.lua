return {
	"monaqa/dial.nvim",
	keys = { "<C-a>", "<C-x>" },
	config = function()
		require("config.keymaps").dial()
	end,
}
