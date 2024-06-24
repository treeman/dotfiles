return {
  "ahmedkhalf/project.nvim",
  config = function()
    require("project_nvim").setup({
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
