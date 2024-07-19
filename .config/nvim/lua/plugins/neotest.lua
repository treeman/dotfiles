return {
  "nvim-neotest/neotest",
  config = function()
    require("neotest").setup({
      log_level = vim.log.levels.DEBUG,
      adapters = {
        require("rustaceanvim.neotest"),
        require("neotest-elixir"),
        require("neotest-plenary"),
      },
    })
  end,
  dependencies = {
    "jfpedroza/neotest-elixir",
    "mrcjkb/rustaceanvim",
    "nvim-neotest/neotest-plenary",
  },
  event = { "BufReadPre", "BufNewFile" },
}
