
vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
	virtual_text = true,
	-- virtual_text = false,
	-- float = { border = "single" },

	-- This is similar to:
	-- let g:diagnostic_show_sign = 1
	-- To configure sign display,
	--  see: ":help vim.lsp.diagnostic.set_signs()"
	signs = true,

	-- Don't show diagnostics while in insert mode
	update_in_insert = false,
})

