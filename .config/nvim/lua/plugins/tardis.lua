require("tardis-nvim").setup({
  keymap = {
    ["next"] = "<Left>", -- next entry in log (older)
    ["prev"] = "<Right>", -- prev entry in log (newer)
    ["quit"] = "q", -- quit all
    ["commit"] = "<C-c>", -- replace contents of origin buffer with contents of tardis buffer
  },
})
