-- TODO :TodoTrouble and :TodoTelescope
local opts = {
  highlight = {
    pattern = [[.*<(KEYWORDS)]], -- Don't require a colon.
  },
  search = {
    pattern = [[\b(KEYWORDS)\b]], -- Match without the extra colon.
  },
}

return {
  "folke/todo-comments.nvim",
  dependencies = { "nvim-lua/plenary.nvim" },
  opts = opts,
  event = { "BufReadPre", "BufNewFile" },
}
