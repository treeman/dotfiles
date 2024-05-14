local cmd = require("util").create_cmd

cmd("UpdateAll", function()
  vim.cmd("MasonUpdate")
  vim.cmd("TSUpdate")
  vim.cmd("Lazy sync")
end)
