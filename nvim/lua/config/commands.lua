local cmd = require("util").create_cmd

cmd("UpdateAll", function()
  vim.cmd("MasonToolsUpdate")
  vim.cmd("TSUpdate")
  vim.cmd("Lazy sync")
end)
