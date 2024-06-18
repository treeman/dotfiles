require("neotest").setup({
  log_level = vim.log.levels.DEBUG,
  adapters = {
    require("rustaceanvim.neotest"),
    require("neotest-elixir"),
  },
})
