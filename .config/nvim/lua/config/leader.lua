-- These must be set before loading plugins
vim.g.mapleader = [[ ]]
if require("util").has_normal_keyboard() then
  vim.g.maplocalleader = [[-]]
else
  vim.g.maplocalleader = [[_]]
end
