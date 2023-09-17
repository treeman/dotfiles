-- I'm not 100% sure I want this plugin, but let's try it out
local opts = {
	current_only = true,
}

return {
	"lewis6991/satellite.nvim",
	opts = opts,
	event = { "BufReadPre", "BufNewFile" },
}
