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
		map("n", prefix .. "d", "<cmd>lua vim.lsp.buf.definition()<CR>")
		map("n", prefix .. "r", "<cmd>lua vim.lsp.buf.references()<CR>")
		map("n", prefix .. "i", "<cmd>lua vim.lsp.buf.implementation()<CR>")
		map("n", prefix .. "t", "<cmd>lua vim.lsp.buf.type_definition()<CR>")
		map("n", prefix .. "h", "<cmd>lua vim.lsp.buf.hover()<CR>")
		map("n", prefix .. "s", "<cmd>lua vim.lsp.buf.signature_help()<CR>")
		map("n", prefix .. "x", "<cmd>lua vim.lsp.buf.code_action()<CR>")
		map("n", prefix .. "l", "<cmd>lua vim.diagnostic.open_float({ focusable = false })<CR>")
		map("n", prefix .. "ar", "<cmd>lua vim.lsp.buf.rename()<CR>")
		map("n", prefix .. "I", "<cmd>lua vim.lsp.buf.incoming_calls()<CR>")
		map("n", prefix .. "O", "<cmd>lua vim.lsp.buf.outgoing_calls()<CR>")
		map("n", prefix .. "w", "<cmd>lua vim.lsp.buf.document_symbol()<CR>")
		map("n", prefix .. "W", "<cmd>lua vim.lsp.buf.workspace_symbol()<CR>")
		map("n", prefix .. "e", "<cmd>:Telescope lsp_document_diagnostics<CR>")
		map("n", prefix .. "E", "<cmd>:Telescope lsp_workspace_diagnostics<CR>")
		-- map('n','<leader>=', '<cmd>lua vim.lsp.buf.formatting()<CR>')
	end

	-- Goto previous/next diagnostic warning/error
	map("n", "]d", "<cmd>lua vim.lsp.diagnostic.goto_next()<CR>")
	map("n", "[d", "<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>")

	-- Omnicompletion support
	vim.api.nvim_command("setlocal omnifunc=v:lua.vim.lsp.omnifunc")

	-- Show diagnostics on hover
	vim.api.nvim_command("setlocal updatetime=150")
	autocmd("Cursorhold", "*", "lua vim.diagnostic.open_float({ focusable = false })")

	autocmd("Cursorhold", "*", "lua require'nvim-lightbulb'.update_lightbulb()")

	-- Enable type inlay hints
	-- Note: This only works for certain LSPs, so it shouldn't be configured here
	-- autocmd('InsertLeave,BufEnter,BufWinEnter,TabEnter,BufWritePost', '*',
	-- "lua require'lsp_extensions'.inlay_hints{prefix = '', highlight = 'Comment'}")
end

-- config that activates keymaps and enables snippet support
local function make_config()
	local capabilities = vim.lsp.protocol.make_client_capabilities()
	capabilities.textDocument.completion.completionItem.snippetSupport = true
	capabilities.textDocument.completion.completionItem.resolveSupport = {
		properties = {
			"documentation",
			"detail",
			"additionalTextEdits",
		},
	}
	return {
		capabilities = capabilities,
		on_attach = custom_attach,
	}
end

-- Register and activate LSP servers (managed by nvim-lsp-installer)
local auto_lsp_servers = {
	-- List name of LSP servers that will be automatically installed and managed by :LspInstall.
	-- LSP servers will be installed locally at: ~/.local/share/nvim/lsp_servers
	-- @see(lspinstall): https://github.com/williamboman/nvim-lsp-installer
	"vimls",
	"tsserver",
	"eslint",
	"sumneko_lua",
	"efm",
	"elixirls",
	"clangd",
	"pylsp",
	"cssls",
	"html",
}

local lsp_setup_opts = {}
lsp_setup_opts["rust_analyzer"] = {
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
lsp_setup_opts["sumneko_lua"] = {
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
}
lsp_setup_opts["clangd"] = {
	filetypes = { "c", "cpp" }, -- we don't want objective-c and objective-cpp!
}
lsp_setup_opts["efm"] = {
	filetypes = { "elixir" },
}

-- lsp-install
local lsp_installer = require("nvim-lsp-installer")

-- FIXME what to do with rust-analyzer and elixirls?
-- Register a handler that will be called for all installed servers.
lsp_installer.on_server_ready(function(server)
	local opts = make_config()

	opts = vim.tbl_extend("error", opts, lsp_setup_opts[server.name] or {})

	-- This setup() function is exactly the same as lspconfig's setup function.
	-- (:help lspconfig-quickstart)
	server:setup(opts)
	vim.cmd([[ do User LspAttachBuffers ]])
end)

-- for _, lsp_name in ipairs(manual_lsp_servers) do
--   local config = make_config()
--   config = vim.tbl_extend("error", config, lsp_setup_opts[lsp_name] or {})
--   require'lspconfig'[lsp_name].setup(config)
-- end

-- Automatically install if a required LSP server is missing.
for _, lsp_name in ipairs(auto_lsp_servers) do
	local ok, lsp = require("nvim-lsp-installer.servers").get_server(lsp_name)
	---@diagnostic disable-next-line: undefined-field
	if ok and not lsp:is_installed() then
		vim.defer_fn(function()
			-- lsp:install()   -- headless
			lsp_installer.install(lsp_name) -- with UI (so that users can be notified)
		end, 0)
	end
end

vim.api.nvim_command("command! LspStop :lua vim.lsp.stop_client(vim.lsp.get_active_clients())<CR>")
vim.api.nvim_command("command! LspStarted :lua print(vim.inspect(vim.lsp.buf_get_clients()))<CR>")

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
	virtual_text = true,

	-- This is similar to:
	-- let g:diagnostic_show_sign = 1
	-- To configure sign display,
	--  see: ":help vim.lsp.diagnostic.set_signs()"
	signs = true,

	-- Don't show diagnostics while in insert mode
	update_in_insert = false,
})

require("telescope").load_extension("lsp_handlers")

-- commented options are defaults
require("lspkind").init({
	with_text = true,
	symbol_map = {
		Text = "",
		Method = "ƒ",
		Function = "",
		Constructor = "",
		Variable = "",
		Class = "",
		Interface = "ﰮ",
		Module = "",
		Property = "",
		Unit = "",
		Value = "",
		Enum = "了",
		Keyword = "",
		Snippet = "﬌",
		Color = "",
		File = "",
		Folder = "",
		EnumMember = "",
		Constant = "",
		Struct = "",
	},
})

require("lsp-rooter").setup({
	-- Table of lsp clients to ignore by name
	ignore_lsp = {},
})

local rust_opts = {
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
	server = vim.tbl_extend("error", make_config(), lsp_setup_opts["rust_analyzer"]),
}
require("rust-tools").setup(rust_opts)

autocmd("FileType", "rust", "nnoremap <leader>x :RustRunnables<CR>")
autocmd("FileType", "rust", "nnoremap <leader>m :RustExpandMacro<CR>")
