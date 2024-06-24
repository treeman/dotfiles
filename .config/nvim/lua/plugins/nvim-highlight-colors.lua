return {
  "brenoprata10/nvim-highlight-colors",
  opts = {
    render = "virtual",
    virtual_symbol = "■",
    enable_named_colors = true,
    enable_tailwind = true,
  },
  event = { "BufReadPost", "BufNewFile" },
}
