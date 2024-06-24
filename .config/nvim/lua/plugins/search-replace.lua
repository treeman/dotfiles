return {
  "roobert/search-replace.nvim",
  config = function()
    require("search-replace").setup({})
  end,
  event = { "BufReadPost", "BufNewFile" },
}
