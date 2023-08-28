local opts = {
  opts = {
    case_sensitive = false,
  }
}

return {
  "ggandor/leap.nvim",
  opts = opts,
  config = function()
    require("leap").add_default_mappings()
  end,
  event = "BufReadPre",
  dependencies = "tpope/vim-repeat",
}
