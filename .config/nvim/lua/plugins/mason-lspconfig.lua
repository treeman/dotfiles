local opts = {
	ensure_installed = {
		"beancount",
		"clangd",
		"clangd",
		"cssls",
		"docker_compose_language_service",
		"dockerls",
		"dotls",
		-- Install managed by elixir-tools instead.
		-- "elixirls",
		"emmet_ls",
		"eslint",
		"html",
		"html",
		"jsonls",
		"lua_ls",
		"marksman",
		"pylsp",
		"rust_analyzer",
		"tsserver",
		"vimls",
		"yamlls",
		"tsserver",
	},
	automatic_installation = true,
}

return {
	"williamboman/mason-lspconfig.nvim",
	opts = opts,
	event = { "BufReadPre", "BufNewFile" },
	dependencies = "williamboman/mason.nvim",
}
