require("trouble").setup({
	auto_jump = { "lsp_definitions", "lsp_references" },
	-- We really should make icons work...
	icons = false,
	fold_open = "v", -- icon used for open folds
	fold_closed = ">", -- icon used for closed folds
	indent_lines = false, -- add an indent guide below the fold icons
	signs = {
		-- icons / text used for a diagnostic
		error = "error",
		warning = "warn",
		hint = "hint",
		information = "info",
	},
	use_diagnostic_signs = false,
})
