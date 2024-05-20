local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup

local group = augroup("pollen.lua", { clear = true })

autocmd({ "BufRead", "BufNewFile" }, {
  pattern = "*html.pm,*html.p,*xml.p,index.ptree",
  callback = function()
    vim.cmd("set filetype=pollen")
    vim.opt_local.wrap = true
    vim.opt_local.ts = 2
    vim.opt_local.sts = 2
    vim.opt_local.sw = 2
    require("config.keymaps").pollen()
  end,
  group = group,
})
