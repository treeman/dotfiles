local mason_lspconfig = require("mason-lspconfig")
local lspconfig = require("lspconfig")

require("mason").setup()

mason_lspconfig.setup({
	ensure_installed = {
		"vimls",
		"tsserver",
		"eslint",
		"lua_ls",
		-- "rust_analyzer",
		-- "efm",
		-- "elixirls",
		"clangd",
		"pylsp",
		-- "cssls",
		"emmet_ls",
		"html",
		-- "tailwindcss",
		"marksman",
	},
})

local capabilities
do
	local capabilities = vim.lsp.protocol.make_client_capabilities()
	capabilities.textDocument.completion.completionItem.snippetSupport = true
	capabilities.textDocument.completion.completionItem.resolveSupport = {
		properties = {
			"documentation",
			"detail",
			"additionalTextEdits",
		},
	}
end

local map = function(type, key, value)
	vim.api.nvim_buf_set_keymap(0, type, key, value, { noremap = true, silent = true })
end

local autocmd = function(event, pat, cmd)
	vim.cmd(table.concat({ "autocmd", event, pat, cmd }, " "))
end

local custom_attach = function(_)
	-- Different keyboard layouts on laptop and main computer
	for _, prefix in ipairs({ "_", "-" }) do
		-- Most here go through telescope via the lsp-handlers plugin
		map("n", prefix .. "D", "<cmd>lua vim.lsp.buf.declaration()<CR>")
		-- map("n", prefix .. "d", "<cmd>lua vim.lsp.buf.definition()<CR>")
		map("n", prefix .. "d", "<cmd>TroubleToggle lsp_definitions<CR>")
		-- map("n", prefix .. "r", "<cmd>lua vim.lsp.buf.references()<CR>")
		map("n", prefix .. "r", "<cmd>TroubleToggle lsp_references<CR>")
		map("n", prefix .. "i", "<cmd>lua vim.lsp.buf.implementation()<CR>")
		map("n", prefix .. "t", "<cmd>lua vim.lsp.buf.type_definition()<CR>")
		map("n", prefix .. "h", "<cmd>lua vim.lsp.buf.hover()<CR>")
		map("n", prefix .. "s", "<cmd>lua vim.lsp.buf.signature_help()<CR>")
		map("n", prefix .. "x", "<cmd>lua vim.lsp.buf.code_action()<CR>")
		map("n", prefix .. "l", "<cmd>lua vim.diagnostic.open_float({ focusable = false })<CR>")
		map("n", prefix .. "ar", "<cmd>lua vim.lsp.buf.rename()<CR>")
		map("n", prefix .. "I", "<cmd>lua vim.lsp.buf.incoming_calls()<CR>")
		map("n", prefix .. "O", "<cmd>lua vim.lsp.buf.outgoing_calls()<CR>")
		-- map("n", prefix .. "w", "<cmd>lua vim.lsp.buf.document_symbol()<CR>")
		-- map("n", prefix .. "W", "<cmd>lua vim.lsp.buf.workspace_symbol()<CR>")
		--
		map("n", prefix .. "e", "<cmd>TroubleToggle document_diagnostics<CR>")
		map("n", prefix .. "w", "<cmd>TroubleToggle workspace_diagnostics<CR>")
		-- map('n','<leader>=', '<cmd>lua vim.lsp.buf.formatting()<CR>')
	end

	-- Goto previous/next diagnostic warning/error
	map("n", "]d", "<cmd>lua vim.diagnostic.goto_next()<CR>")
	map("n", "[d", "<cmd>lua vim.diagnostic.goto_prev()<CR>")

	-- Omnicompletion support
	vim.api.nvim_command("setlocal omnifunc=v:lua.vim.lsp.omnifunc")

	-- Show diagnostics on hover
	vim.api.nvim_command("setlocal updatetime=150")
	autocmd("Cursorhold", "*", "lua vim.diagnostic.open_float({ focusable = false })")

	autocmd("Cursorhold", "*", "lua require'nvim-lightbulb'.update_lightbulb()")
end

local lsp_defaults = {
	capabilities = capabilities,
	on_attach = custom_attach,
}

lspconfig.util.default_config = vim.tbl_deep_extend("force", lspconfig.util.default_config, lsp_defaults)

-- See :help mason-lspconfig-dynamic-server-setup
mason_lspconfig.setup_handlers({
	function(server)
		lspconfig[server].setup({})
	end,
	["lua_ls"] = function()
		lspconfig.lua_ls.setup({
			settings = {
				Lua = {
					runtime = {
						-- LuaJIT in the case of Neovim
						version = "LuaJIT",
						path = vim.split(package.path, ";"),
					},
					diagnostics = {
						-- Get the language server to recognize the `vim` global
						globals = { "vim" },
					},
					workspace = {
						-- Make the server aware of Neovim runtime files
						library = {
							[vim.fn.expand("$VIMRUNTIME/lua")] = true,
							[vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true,
						},
					},
				},
			},
		})
	end,
	["clangd"] = function()
		lspconfig.clangd.setup({
			filetypes = { "c", "cpp" }, -- we don't want objective-c and objective-cpp!
		})
	end,
	-- ["elixirls"] = function()
	-- 	lspconfig.elixirls.setup({
	-- 		cmd = { "/home/tree/src/elixir-ls/release/language_server.sh" },
	-- 		dialyzerEnabled = true,
	-- 		fetchDeps = true,
	-- 		enableTestLenses = true,
	-- 		suggestSpecs = true,
	-- 	})
	-- end,
})

vim.api.nvim_command("command! LspStop :lua vim.lsp.stop_client(vim.lsp.get_active_clients())<CR>")
vim.api.nvim_command("command! LspStarted :lua print(vim.inspect(vim.lsp.buf_get_clients()))<CR>")

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
	-- virtual_text = true,
	virtual_text = false,
	float = { border = "single" },

	-- This is similar to:
	-- let g:diagnostic_show_sign = 1
	-- To configure sign display,
	--  see: ":help vim.lsp.diagnostic.set_signs()"
	signs = true,

	-- Don't show diagnostics while in insert mode
	update_in_insert = false,
})

require("telescope").load_extension("lsp_handlers")

require("lsp-rooter").setup({
	-- Table of lsp clients to ignore by name
	ignore_lsp = { "elixirls" },
})

-- rust-analyzer is managed outside of mason
local rust_opts = {
	capabilities = capabilities,
	on_attach = custom_attach,
	-- rust-tools options
	tools = {
		autoSetHints = true,
		inlay_hints = {
			show_parameter_hints = true,
			parameter_hints_prefix = "ρ ",
			other_hints_prefix = "τ ",
		},
	},
	-- rust-analyzer options
	settings = {
		["rust-analyzer"] = {
			diagnostics = {
				-- Disables 'proc macro `Serialize` not expanded and similar
				-- https://github.com/rust-analyzer/rust-analyzer/pull/6645
				disabled = { "unresolved-proc-macro" },
			},
			checkOnSave = {
				extraArgs = { "--target-dir", "/tmp/rust-analyzer-check" },
				command = "clippy",
			},
		},
	},
}
require("rust-tools").setup(rust_opts)

autocmd("FileType", "rust", "nnoremap <leader>x :RustRunnables<CR>")
autocmd("FileType", "rust", "nnoremap <leader>m :RustExpandMacro<CR>")

-- elixirls is managed outside of mason
lspconfig.elixirls.setup({
	cmd = { "/home/tree/src/elixir-ls/release/language_server.sh" },
	dialyzerEnabled = false,
	fetchDeps = true,
	enableTestLenses = false,
	suggestSpecs = false,
})

local elixir = require("elixir")
local elixirls = require("elixir.elixirls")

elixir.setup({
	credo = {
		capabilities = capabilities,
		on_attach = custom_attach,
	},
	elixirls = {
		enable = false,
	},
	-- elixirls = {
	-- 	cmd = { "/home/tree/src/elixir-ls/release/language_server.sh" },
	-- 	-- default settings, use the `settings` function to override settings
	-- 	settings = elixirls.settings({
	-- 		dialyzerEnabled = true,
	-- 		fetchDeps = true,
	-- 		enableTestLenses = true,
	-- 		suggestSpecs = true,
	-- 	}),
	-- 	capabilities = capabilities,
	-- 	on_attach = custom_attach,
	-- },
})
