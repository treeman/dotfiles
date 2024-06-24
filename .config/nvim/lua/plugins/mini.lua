local function config()
  require("mini.ai").setup({
    custom_textobjects = {
      -- Prefer existing textobjects
      a = false,
      f = false,
      t = false,

      -- Whole buffer
      g = function()
        local from = { line = 1, col = 1 }
        local to = {
          line = vim.fn.line("$"),
          col = math.max(vim.fn.getline("$"):len(), 1),
        }
        return { from = from, to = to }
      end,
    },
  })

  require("mini.cursorword").setup({})

  require("mini.trailspace").setup({})

  require("mini.git").setup({})
end

return {
  "echasnovski/mini.nvim",
  version = false,
  config = config,
  event = { "BufReadPost", "BufNewFile" },
  cmd = "Git",
}
