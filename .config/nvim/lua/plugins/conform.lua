local opts = {
	formatters_by_ft = {
		lua = { "stylua" },
		javascript = { { "prettierd", "prettier" } },
		typescript = { { "prettierd", "prettier" } },
		css = { { "prettierd", "prettier" } },
		scss = { { "prettierd", "prettier" } },
		less = { { "prettierd", "prettier" } },
		html = { { "prettierd", "prettier" } },
		json = { { "prettierd", "prettier" } },
		yaml = { { "prettierd", "prettier" } },
		toml = { { "prettierd", "prettier" } },
		rust = { "rustfmt" },
		sql = { "pg_format" },
	},
	format_on_save = function(bufnr)
		-- Disable autoformat on certain filetypes
		-- local ignore_filetypes = { "python" }
		-- if vim.tbl_contains(ignore_filetypes, vim.bo[bufnr].filetype) then
		-- 	return
		-- end
		-- Disable with a global or buffer-local variable
		if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
			return
		end
		-- Disable autoformat for files in a certain path
		local bufname = vim.api.nvim_buf_get_name(bufnr)
		if bufname:match("/euronetics/") then
			return
		end
		return { timeout_ms = 500, lsp_fallback = true }
	end,
}

return {
	"stevearc/conform.nvim",
	event = "BufWritePre",
	cmd = "ConformInfo",
	opts = opts,
}
