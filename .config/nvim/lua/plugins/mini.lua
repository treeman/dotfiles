local version = false

return {
  {
    "echasnovski/mini.cursorword",
    event = { "BufReadPre", "BufNewFile" },
    version = version,
    opts = true,
  },
  {
    "echasnovski/mini.trailspace",
    event = { "BufReadPre", "BufNewFile" },
    version = version,
    opts = true,
  },
  {
    "echasnovski/mini.ai",
    event = { "BufReadPre", "BufNewFile" },
    version = version,
    opts = {
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
    },
  },
}
