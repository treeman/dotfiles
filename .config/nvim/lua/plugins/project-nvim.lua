return {
  "ahmedkhalf/project.nvim",
  config = function()
    require("project_nvim").setup({
      ignore_lsp = { "ts_ls", "cssls" },
      patterns = {
        ".git",
        "_darcs",
        ".hg",
        ".bzr",
        ".svn",
        "Makefile",
        "package.json",
        "rocks.toml",
        "lazy-lock.json",
      },
    })
  end,
  event = { "BufReadPost", "BufNewFile" },
}
