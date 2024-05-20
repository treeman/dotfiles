local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup

autocmd({ "BufReadPre", "BufNewFile" }, {
  pattern = "*",
  group = augroup("lazy_plugins.lspconfig", { clear = true }),
  callback = function(opts)
    require("lazy_plugins.lspconfig")
    -- Only do setup once... But setup is fast enough so it really doesn't matter.
    -- vim.api.nvim_del_augroup_by_name("lazy_plugins.lspconfig")
  end,
})
