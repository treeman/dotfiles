local function get_lsp_fallback() end

local opts = {
	formatters_by_ft = {
		lua = { "stylua" },
		javascript = { "prettierd", "prettier" },
		typescript = { "prettierd", "prettier" },
		css = { "prettierd", "prettier" },
		scss = { "prettierd", "prettier" },
		less = { "prettierd", "prettier" },
		html = { "prettierd", "prettier" },
		json = { "prettierd", "prettier" },
		yaml = { "prettierd", "prettier" },
		rust = { "rustfmt" },
		-- This breaks my custom attributes in my blog
		-- markdown = { "prettierd", "prettier" },
	},
	format_on_save = {
		timeout_ms = 500,
		lsp_fallback = true,
	},
	-- format_afterrsave = function(bufnr)
	-- 	return {
	-- 		lsp_fallback = get_lsp_fallback(bufnr),
	-- 	}
	-- end,
}

return {
	"stevearc/conform.nvim",
	event = "BufWritePre",
	opts = opts,
}
