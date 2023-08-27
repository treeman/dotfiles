return {
  {
    "sudormrfbin/cheatsheet.nvim",
    dependencies = {
      'nvim-telescope/telescope.nvim',
      'nvim-lua/popup.nvim',
      'nvim-lua/plenary.nvim',
    },
    opts = {
      bundled_cheatsheets = false,
    },
    cmd = {
      "Cheatsheet",
      "CheatsheetEdit",
    },
    keys = {
      { "<leader>?", "<cmd>Cheatsheet<cr>", desc = "Cheatsheet" }
    },
  }
}
