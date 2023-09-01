local opts = {
	opts = {
		case_sensitive = false,
	},
}

return {
	"ggandor/leap.nvim",
	opts = opts,
	config = function()
		require("leap").add_default_mappings()

		-- FIXME IncSearch might not be what we really want
		vim.api.nvim_set_hl(0, "LeapLabelPrimary", { link = "IncSearch" })
	end,
	event = "BufReadPre",
	dependencies = "tpope/vim-repeat",
}
