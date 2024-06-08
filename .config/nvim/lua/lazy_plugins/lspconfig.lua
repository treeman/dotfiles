local cmp_nvim_lsp = require("cmp_nvim_lsp")
local keymaps = require("config.keymaps")
local lsp_status = require("lsp-status")
local lspconfig = require("lspconfig")
local mason_lspconfig = require("mason-lspconfig")

require("mason").setup({})

mason_lspconfig.setup({
  ensure_installed = {
    -- "beancount",
    "clangd",
    "clangd",
    "cssls",
    "docker_compose_language_service",
    "dockerls",
    "dotls",
    -- Install managed by elixir-tools instead.
    -- "elixirls",
    -- "nextls",
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

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local bufnr = args.buf
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    keymaps.buf_lsp(client, bufnr)
    lsp_status.on_attach(client)
  end,
})

vim.diagnostic.config({
  virtual_text = false,
  severity_sort = true,
  float = {
    scope = "cursor",
  },
})

keymaps.global_lsp()

vim.lsp.inlay_hint.enable(true)

lspconfig.util.default_config = vim.tbl_deep_extend("force", lspconfig.util.default_config, {
  capabilities = capabilities,
})

-- This compiles the LSP using the exact Elixir + Erlang version, while giving us some extra functionality.
require("elixir").setup({
  nextls = {
    enable = false,
    init_options = {
      mix_env = "dev",
      experimental = {
        completions = {
          enable = true,
        },
      },
    },
    capabilities = capabilities,
  },
  credo = { enable = true, capabilities = capabilities },
  elixirls = {
    enable = true,
    settings = require("elixir.elixirls").settings({
      dialyzerEnabled = true,
      enableTestLenses = true,
      suggestSpecs = true,
      fetchDeps = true,
    }),
    capabilities = capabilities,
  },
})

vim.g.rustaceanvim = {
  -- Plugin configuration
  tools = {},
  -- LSP configuration
  server = {
    default_settings = {
      -- rust-analyzer language server configuration
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
  ["clangd"] = function()
    lspconfig.clangd.setup({
      filetypes = { "c", "cpp" }, -- we don't want objective-c and objective-cpp!
    })
  end,
  ["tsserver"] = function()
    require("typescript-tools").setup({
      capabilities = capabilities,
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
