return {
  "FabijanZulj/blame.nvim",
  cmd = "BlameToggle",
  config = function()
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
      colors = {
        "#867462",
        "#D47766",
        "#EBC06D",
        "#85B695",
        "#A3A9CE",
        "#CF9BC2",
        "#BD8183",
        "#E49B5D",
        "#78997A",
        "#7F91B2",
        "#B380B0",
      },
    })
  end,
}
