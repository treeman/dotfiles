-- Some special mappings for my T-34 layout shenanigans
local normal_keyboard = os.getenv("NORMAL_KEYBORD") == 1

-- These must be set before loading plugins
vim.g.mapleader = [[ ]]
if normal_keyboard then
  vim.g.maplocalleader = [[-]]
else
  vim.g.maplocalleader = [[_]]
end

