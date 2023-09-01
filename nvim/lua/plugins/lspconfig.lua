local config = function()
	local lspconfig = require("lspconfig")
	local mason_lspconfig = require("mason-lspconfig")
	local cmp_nvim_lsp = require("cmp_nvim_lsp")
	local neodev = require("neodev")
	local lsp_status = require("lsp-status")

	local capabilities = vim.lsp.protocol.make_client_capabilities()
	capabilities = cmp_nvim_lsp.default_capabilities(capabilities)
	capabilities = vim.tbl_extend("keep", capabilities, lsp_status.capabilities)
	capabilities.textDocument.completion.completionItem.snippetSupport = true

	vim.diagnostic.config({
		-- Disable as we use lsp_lines instead
		virtual_text = false,
	})

	-- global keybindings
	local key_opts = { noremap = true, silent = true }
	vim.keymap.set("n", "]d", vim.diagnostic.goto_next, key_opts)
	vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, key_opts)

	-- Local keybindings
	local keys_on_attach = function(_, buffer)
		-- FIXME there are other cool possibilities listed in nvim-lspconfig
		local opts = { noremap = true, silent = true, buffer = buffer }
		vim.keymap.set("n", "<localleader>D", vim.lsp.buf.declaration, opts)
		vim.keymap.set("n", "<localleader>d", vim.lsp.buf.definition, opts)
		vim.keymap.set("n", "<localleader>r", vim.lsp.buf.references, opts)
		vim.keymap.set("n", "<localleader>i", vim.lsp.buf.implementation, opts)
		vim.keymap.set("n", "<localleader>t", vim.lsp.buf.type_definition, opts)
		vim.keymap.set("n", "<localleader>h", vim.lsp.buf.hover, opts)
		vim.keymap.set("n", "<localleader>s", vim.lsp.buf.signature_help, opts)
		vim.keymap.set("n", "<localleader>x", vim.lsp.buf.code_action, opts)
		-- map("n", prefix .. "l", "<cmd>lua vim.diagnostic.open_float({ focusable = false })<CR>")
		vim.keymap.set("n", "<localleader>ar", vim.lsp.buf.rename, opts)
		vim.keymap.set("n", "<localleader>I", vim.lsp.buf.incoming_calls, opts)
		vim.keymap.set("n", "<localleader>O", vim.lsp.buf.outgoing_calls, opts)
		vim.keymap.set("n", "<localleader>w", vim.lsp.buf.document_symbol, opts)
		vim.keymap.set("n", "<localleader>W", vim.lsp.buf.workspace_symbol, opts)

		-- Trouble is okay... But we really don't want it to steal focus!
		-- map("n", prefix .. "r", "<cmd>TroubleToggle lsp_references<CR>")
		-- map("n", prefix .. "d", "<cmd>TroubleToggle lsp_definitions<CR>")
		-- map("n", prefix .. "e", "<cmd>TroubleToggle document_diagnostics<CR>")
		-- map("n", prefix .. "w", "<cmd>TroubleToggle workspace_diagnostics<CR>")
	end

	-- Formatting using LSP seems very janky. Should use another instead...
	-- local format_group = vim.api.nvim_create_augroup("LspFormatting", {})
	-- local format_on_attach = function(client, buffer)
	--   if client.supports_method("textDocument/formatting") then
	--     vim.api.nvim_clear_autocmds({ group = format_group, buffer = buffer })
	--     vim.api.nvim_create_autocmd("BufWritePre", {
	--       group = format_group,
	--       buffer = buffer,
	--       callback = function()
	--         vim.lsp.buf.format({
	--           bufnr = buffer,
	--         })
	--       end,
	--     })
	--   end
	-- end

	local on_attach = function(client, buffer)
		keys_on_attach(client, buffer)
		-- format_on_attach(client, buffer)
		lsp_status.on_attach(client)
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

		-- Should consider...
		-- "windwp/nvim-autopairs",
		-- "kevinhwang91/nvim-ufo",
		-- "VidocqH/lsp-lens.nvim",
		-- "jubnzv/virtual-types.nvim",
	},
}
