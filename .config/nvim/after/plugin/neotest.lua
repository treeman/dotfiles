require("neotest").setup({
  adapters = {
    require("rustaceanvim.neotest"),
    -- require("neotest-vim-test")({ ignore_filetypes = { "rust" } }),
    -- FIXME doesn't find tests in a file!
    require("neotest-elixir"),
  },
})
