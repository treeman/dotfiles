vim.api.nvim_create_autocmd({ "BufReadPre", "BufNewFile" }, {
  pattern = "*",
  callback = function()
    require("lazy_plugins.lspconfig")
    return true
  end,
})
