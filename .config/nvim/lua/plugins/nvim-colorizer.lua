local opts = {
	user_default_options = {
		names = false,
		mode = "virtualtext",
	},
	filetypes = {
		"*",
		css = { css = true, tailwind = true },
		scss = { css = true, tailwind = true },
	},
}

return {
	"NvChad/nvim-colorizer.lua",
	event = { "BufReadPre", "BufNewFile" },
	opts = opts,
}
