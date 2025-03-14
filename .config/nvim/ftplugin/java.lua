local config = {
  cmd = { vim.fn.expand("~/src/eclipse_jdt/bin/jdtls") },
  root_dir = vim.fs.dirname(vim.fs.find({ "gradlew", ".git", "mvnw" }, { upward = true })[1]),
  settings = {
    java = {
      format = {
        comments = { enabled = false },
        insertSpaces = false,
        tabSize = 4,
        onType = { enabled = false },
        settings = { url = vim.fn.expand("~/.config/nvim/ext/jldts-settings.xml") },
      },
    },
  },
}
require("jdtls").start_or_attach(config)

vim.cmd("setlocal ts=4 sts=4 sw=4 expandtab")
