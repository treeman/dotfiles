local opts = {
  ensure_installed = {
    -- Install managed by elixir-tools instead.
    -- "elixirls",
    -- "nextls",

    -- Only "necessary"
    "lua_ls",
    -- Can ask for everything else
    -- "clangd",
    -- "clangd",
    -- "cssls",
    -- "docker_compose_language_service",
    -- "dockerls",
    -- "dotls",
    -- "emmet_ls",
    -- "eslint",
    -- "html",
    -- "html",
    -- "jsonls",
    -- "marksman",
    -- "pylsp",
    -- "rust_analyzer",
    -- "tsserver",
    -- "vimls",
    -- "yamlls",
  },
  automatic_installation = true,
  automatic_enable = {
    exclude = {
      "rust_analyzer",
      "jdtls",
    },
  },
}

return {
  "williamboman/mason-lspconfig.nvim",
  opts = opts,
  dependencies = "williamboman/mason.nvim",
}
