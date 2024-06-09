vim.keymap.set("n", "<leader>x", function()
  vim.lsp.codelens.run()
end)

-- FIXME neotest doesn't work with Elixir for some reason
-- require("config.keymaps").neotest(0)
