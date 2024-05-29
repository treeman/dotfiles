require("nio").run(function()
  -- Maybe there's a better way to find the package location?
  local package_path =
    vim.fs.joinpath(vim.fn.stdpath("data"), "site/pack/rocks/start", "telescope-fzf-native.nvim/")

  -- Before loading the extension we need to build it with `make`.
  require("nio").process.run({
    cmd = "make",
    args = {},
    cwd = package_path,
  })
  require("telescope").load_extension("fzf")
end)
