local opts = {
	case_sensitive = false,
}

return {
	"ggandor/leap.nvim",
	opts = opts,
	config = function()
		local leap = require("leap")
		leap.opts = opts
		leap.add_default_mappings()
	end,
	event = { "BufReadPre", "BufNewFile" },
	dependencies = "tpope/vim-repeat",
	enabled = false,
}
