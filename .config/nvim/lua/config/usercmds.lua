local create_cmd = require("util.helpers").create_cmd

create_cmd("ToggleSpell", function()
  vim.opt.spell = not (vim.opt.spell:get())
end)

create_cmd("Rename", function()
  require("snacks").rename.rename_file()
end)
