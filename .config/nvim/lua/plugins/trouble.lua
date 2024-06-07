require("trouble").setup({
  modes = {
    telescope = {
      sort = { "pos", "filename", "severity", "message" },
    },
    quickfix = {
      sort = { "pos", "filename", "severity", "message" },
    },
    loclist = {
      sort = { "pos", "filename", "severity", "message" },
    },
    todo = {
      sort = { "pos", "filename", "severity", "message" },
    },
    cascade = {
      mode = "diagnostics", -- inherit from diagnostics mode
      filter = function(items)
        local severity = vim.diagnostic.severity.HINT
        for _, item in ipairs(items) do
          severity = math.min(severity, item.severity)
        end
        return vim.tbl_filter(function(item)
          return item.severity == severity
        end, items)
      end,
    },
  },
})

require("custom.trouble")
require("custom.ts_trouble")
