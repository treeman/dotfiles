local config = function()
	local lspconfig = require("lspconfig")
	local mason_lspconfig = require("mason-lspconfig")
	local cmp_nvim_lsp = require("cmp_nvim_lsp")
	local neodev = require("neodev")
	local lsp_status = require("lsp-status")
	local keymaps = require("config.keymaps")

	local capabilities = vim.lsp.protocol.make_client_capabilities()
	capabilities = cmp_nvim_lsp.default_capabilities(capabilities)
	capabilities = vim.tbl_extend("keep", capabilities, lsp_status.capabilities)
	capabilities.textDocument.completion.completionItem.snippetSupport = true

	vim.diagnostic.config({
		-- Disable as we use lsp_lines instead
		virtual_text = false,
	})

	-- global keybindings
	keymaps.global_lsp()

	local on_attach = function(client, buffer)
		keymaps.buf_lsp(client, buffer)
		lsp_status.on_attach(client)

		if client.server_capabilities.inlayHintProvider then
			vim.lsp.inlay_hint(buffer, true)
		end
	end

	lspconfig.util.default_config = vim.tbl_deep_extend("force", lspconfig.util.default_config, {
		capabilities = capabilities,
		on_attach = on_attach,
	})

	-- elixirls is managed by elixir-tools outside of mason.
	-- This compiles the LSP using the exact Elixir + Erlang version, while giving us some extra functionality.
	local elixir = require("elixir")
	local elixirls = require("elixir.elixirls")
	elixir.setup({
		nextls = { enable = false },
		credo = { enable = true, capabilities = capabilities, on_attach = on_attach },
		elixirls = {
			enable = true,
			settings = elixirls.settings({
				dialyzerEnabled = true,
				enableTestLenses = true,
				suggestSpecs = true,
				fetchDeps = true,
			}),
			capabilities = capabilities,
			-- FIXME extra commands available?
			-- vim.lsp.codelens.run()
			-- :ElixirFromPipe
			-- :ElixirToPipe
			-- :ElixirExpandMacro
			-- :ElixirOutputPanel
			-- :Mix
			on_attach = on_attach,
		},
	})

	-- Neodev must come before lsp.
	-- Will override lua_ls settings, but only for Neovim config files.
	neodev.setup({})

	-- Dynamic server setup, so we don't have to explicitly list every single server
	-- and can just list the ones we want to override configuration for.
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
						-- Do not send telemetry data containing a randomized but unique identifier
						telemetry = {
							enable = false,
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
		-- FIXME extra commands available?
		-- :RustRunnables
		-- :RustExpandMacro
		["rust_analyzer"] = function()
			require("rust-tools").setup({
				capabilities = capabilities,
				on_attach = on_attach,
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
			})
		end,
		["tsserver"] = function()
			require("typescript-tools").setup({
				capabilities = capabilities,
				on_attach = on_attach,
				settings = {
					expose_as_code_action = "all",
					tsserver_file_preferences = {
						includeCompletionsForModuleExports = true,
						includeCompletionsForImportStatements = true,
						includeCompletionsWithObjectLiteralMethodSnippets = true,

						includeInlayParameterNameHints = "all",
						includeInlayParameterNameHintsWhenArgumentMatchesName = true,
						includeInlayFunctionParameterTypeHints = true,
						includeInlayVariableTypeHints = true,
						includeInlayPropertyDeclarationTypeHints = true,
						includeInlayVariableTypeHintsWhenTypeMatchesName = true,
						includeInlayFunctionLikeReturnTypeHints = true,
						includeInlayEnumMemberValueHints = true,

						importModuleSpecifierPreference = 'non-relative',
						quotePreference = "auto",
					},
				}
			})
		end
	})
end

return {
	"neovim/nvim-lspconfig",
	config = config,
	dependencies = {
		"williamboman/mason.nvim",
		"onsails/lspkind-nvim",
		"ahmedkhalf/lsp-rooter.nvim",
		"litao91/lsp_lines",
		"hrsh7th/cmp-nvim-lsp",
		"simrat39/rust-tools.nvim",
		"elixir-tools/elixir-tools.nvim",
		"kosayoda/nvim-lightbulb",
		"folke/neodev.nvim",
		"nvim-lua/lsp-status.nvim",
		{
			"pmizio/typescript-tools.nvim",
			dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
			config = false,
		}

		-- Should consider...
		-- "windwp/nvim-autopairs",
		-- "kevinhwang91/nvim-ufo",
		-- "VidocqH/lsp-lens.nvim",
		-- "jubnzv/virtual-types.nvim",
	},
}
