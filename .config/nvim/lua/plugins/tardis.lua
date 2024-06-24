return {
  "fredeeb/tardis.nvim",
  dependencies = { "nvim-lua/plenary.nvim" },
  opts = {
    keymap = {
      ["next"] = "<Left>", -- next entry in log (older)
      ["prev"] = "<Right>", -- prev entry in log (newer)
      ["quit"] = "q", -- quit all
      ["commit"] = "<C-c>", -- replace contents of origin buffer with contents of tardis buffer
    },
  },
  cmd = "Tardis",
}
