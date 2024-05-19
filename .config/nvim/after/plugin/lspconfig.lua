local cmp_nvim_lsp = require("cmp_nvim_lsp")
local keymaps = require("config.keymaps")
local lsp_status = require("lsp-status")
local lspconfig = require("lspconfig")
local mason = require("mason")
local mason_lspconfig = require("mason-lspconfig")
local neodev = require("neodev")

mason.setup({})

mason_lspconfig.setup({
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
    -- "nextls", -- Can't install automatically, even though we can install it using :Mason ?
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
})

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = cmp_nvim_lsp.default_capabilities(capabilities)
capabilities = vim.tbl_extend("keep", capabilities, lsp_status.capabilities)
capabilities.textDocument.completion.completionItem.snippetSupport = true

vim.diagnostic.config({
  -- Disable as we use lsp_lines instead
  virtual_text = false,
})

-- Setup global keybindings.
keymaps.global_lsp()

vim.lsp.inlay_hint.enable(true)

local on_attach = function(client, buffer)
  keymaps.buf_lsp(client, buffer)
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
  nextls = {
    enable = false,
    cmd = vim.fn.expand("$HOME/.local/share/nvim/mason/bin/nextls"),
    capabilities = capabilities,
    on_attach = on_attach,
  },
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

vim.g.rustaceanvim = {
  -- Plugin configuration
  tools = {},
  -- LSP configuration
  server = {
    on_attach = on_attach,
    default_settings = {
      -- rust-analyzer language server configuration
      ["rust-analyzer"] = {},
    },
  },
  -- DAP configuration
  dap = {},
}

-- Dynamic server setup, so we don't have to explicitly list every single server
-- and can just list the ones we want to override configuration for.
-- See :help mason-lspconfig-dynamic-server-setup
mason_lspconfig.setup_handlers({
  function(server)
    -- Depend on rustaceanvim to setup `rust_analyzer`.
    -- We're not allowed to call `setup` ourselves.
    if server ~= "rust_analyzer" then
      lspconfig[server].setup({})
    end
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

          importModuleSpecifierPreference = "non-relative",
          quotePreference = "auto",
        },
      },
    })
  end,
})
