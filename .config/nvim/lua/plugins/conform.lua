local opts = {
	formatters_by_ft = {
		lua = { "stylua" },
		javascript = { "prettier_d", "prettier" },
		typescript = { "prettier_d", "prettier" },
		css = { "prettier_d", "prettier" },
		scss = { "prettier_d", "prettier" },
		less = { "prettier_d", "prettier" },
		html = { "prettier_d", "prettier" },
		json = { "prettier_d", "prettier" },
		markdown = { "prettier_d", "prettier" },
		yaml = { "prettier_d", "prettier" },
		rust = { "rustfmt" },
	},
	format_on_save = {
		timeout_ms = 500,
		lsp_fallback = true,
	},
}

return {
	"stevearc/conform.nvim",
	event = "BufWritePre",
	opts = opts,
}
