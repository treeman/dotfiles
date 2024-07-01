return {
  "tpope/vim-dadbod",
  event = { "BufReadPost", "BufNewFile" },
  cmd = { "DB", "DBUI", "DBCompletionClearCache", "DBUIAddConnection", "DBUIToggle" },
  dependencies = {
    "kristijanhusak/vim-dadbod-completion",
    "kristijanhusak/vim-dadbod-ui",
  },
}
