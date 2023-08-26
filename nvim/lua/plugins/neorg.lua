local opts = {
  load = {
    ["core.defaults"] = {},
    ["core.completion"] = { config = { engine = "nvim-cmp" } },
    ["core.integrations.nvim-cmp"] = {},
    ["core.dirman"] = {
      config = {
        workspaces = {
          norg = "~/norg",
          projects = "~/norg/projects",
          areas = "~/norg/areas",
          resources = "~/norg/resources",
        },
      },
    },
    -- ["core.journal"] = { config = { workspace = "norg", strategy = "flat" } },
    ["core.concealer"] = {
      config = {
        icons = {
          todo = {
            done = { icon = "✓" },
            pending = { icon = "▶" },
            uncertain = { icon = "⁇" },
            on_hold = { icon = "⏸" },
            cancelled = { icon = "⏏" },
            undone = { icon = " " },
          },
        },
      },
    },
  },
}

return {
    {
        "nvim-neorg/neorg",
        -- lazy-load on filetype
        ft = "norg",
        opts = opts
    }
}
