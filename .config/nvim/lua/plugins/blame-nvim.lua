require("blame").setup({
  date_format = "%Y-%m-%d",
  merge_consecutive = true,
  format_fn = require("blame.formats.default_formats").date_message,
  mappings = {
    commit_info = "i",
    stack_push = "<Left>",
    stack_pop = "<Right>",
    show_commit = "<CR>",
    close = { "<esc>", "q" },
  },
})
