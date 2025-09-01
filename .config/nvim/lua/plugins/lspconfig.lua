local function config()
  local cmp_nvim_lsp = require("cmp_nvim_lsp")
  local keymaps = require("config.keymaps")
  local lsp_status = require("lsp-status")
  local lspconfig = require("lspconfig")
  local create_cmd = require("util.helpers").create_cmd

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

  create_cmd("ToggleInlayHints", function()
    vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = 0 }), { bufnr = 0 })
  end)

  lspconfig.util.default_config = vim.tbl_deep_extend("force", lspconfig.util.default_config, {
    capabilities = capabilities,
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
          -- checkOnSave = {
          --   extraArgs = { "--target-dir", "/tmp/rust-analyzer-check" },
          --   command = "clippy",
          -- },
        },
      },
    },
    -- DAP configuration
    dap = {},
  }

  -- The Gleam LSP is integrated into the gleam cli and isn't installed via mason.
  lspconfig.gleam.setup({})

  vim.lsp.config("clangd", {
    filetypes = { "c", "cpp" }, -- we don't want objective-c and objective-cpp!
  })

  vim.lsp.config("expert", {
    cmd = { "expert" },
    root_markers = { "mix.exs", ".git" },
    filetypes = { "elixir", "eelixir", "heex" },
  })

  vim.lsp.enable("expert")

  -- Dynamic server setup, so we don't have to explicitly list every single server
  -- and can just list the ones we want to override configuration for.
  -- See :help mason-lspconfig-dynamic-server-setup
  -- mason_lspconfig.setup_handlers({
  --   end,
  --   -- ["tsserver"] = function()
  --   --   require("typescript-tools").setup({
  --   --     capabilities = capabilities,
  --   --     settings = {
  --   --       expose_as_code_action = "all",
  --   --       tsserver_file_preferences = {
  --   --         includeCompletionsForModuleExports = true,
  --   --         includeCompletionsForImportStatements = true,
  --   --         includeCompletionsWithObjectLiteralMethodSnippets = true,
  --   --
  --   --         includeInlayParameterNameHints = "all",
  --   --         includeInlayParameterNameHintsWhenArgumentMatchesName = true,
  --   --         includeInlayFunctionParameterTypeHints = true,
  --   --         includeInlayVariableTypeHints = true,
  --   --         includeInlayPropertyDeclarationTypeHints = true,
  --   --         includeInlayVariableTypeHintsWhenTypeMatchesName = true,
  --   --         includeInlayFunctionLikeReturnTypeHints = true,
  --   --         includeInlayEnumMemberValueHints = true,
  --   --
  --   --         importModuleSpecifierPreference = "non-relative",
  --   --         quotePreference = "auto",
  --   --       },
  --   --     },
  --   --   })
  --   -- end,
  -- })
end

return {
  "neovim/nvim-lspconfig",
  config = config,
  dependencies = {
    "williamboman/mason-lspconfig.nvim",
    -- "onsails/lspkind-nvim",
    "ahmedkhalf/lsp-rooter.nvim",
    "hrsh7th/cmp-nvim-lsp",
    {
      "mrcjkb/rustaceanvim",
      version = "^5", -- Recommended
      lazy = false, -- This plugin is already lazy
    },
    "kosayoda/nvim-lightbulb",
    "nvim-lua/lsp-status.nvim",
    {
      "pmizio/typescript-tools.nvim",
      dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
      config = false,
    },

    -- Should consider...
    -- "windwp/nvim-autopairs",
    -- "kevinhwang91/nvim-ufo",
    -- "VidocqH/lsp-lens.nvim",
    -- "jubnzv/virtual-types.nvim",
  },
  event = { "BufReadPre", "BufNewFile" },
}
