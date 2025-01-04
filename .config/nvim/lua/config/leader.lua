-- These must be set before loading plugins
vim.g.mapleader = [[ ]]
if require("util.keyboard").has_custom_keyboard_layout() then
  vim.g.maplocalleader = [[_]]
else
  vim.g.maplocalleader = [[-]]
end
