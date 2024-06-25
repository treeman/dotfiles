return {
  "rcarriga/nvim-notify",
  config = function()
    local notify = require("notify")
    vim.notify = notify
    notify.setup({
      render = "compact",
      stages = "static",
      timeout = 3000,
    })
  end,
  lazy = false,
}
