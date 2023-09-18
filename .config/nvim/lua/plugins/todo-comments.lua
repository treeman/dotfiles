-- TODO :TodoTrouble and :TodoTelescope
local opts = {
	highlight = {
		-- If we don't want spaces between to be highlighted
		-- before = "",
		-- keyword = "bg",
		-- after = "",
		pattern = [[.*<(KEYWORDS)]], -- Don't require a colon.
	},
	search = {
		pattern = [[\b(KEYWORDS)\b]], -- Match without the extra colon.
	},
}

return {
	"folke/todo-comments.nvim",
	dependencies = { "nvim-lua/plenary.nvim" },
	opts = opts,
	event = { "BufReadPre", "BufNewFile" },
}
