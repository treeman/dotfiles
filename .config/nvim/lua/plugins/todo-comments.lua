return {
  "folke/todo-comments.nvim",
  opts = {
    highlight = {
      pattern = [[.*<(KEYWORDS)]], -- Don't require a colon.
    },
    search = {
      pattern = [[\b(KEYWORDS)\b]], -- Match without the extra colon.
    },
  },
  event = { "BufReadPost", "BufNewFile" },
}
