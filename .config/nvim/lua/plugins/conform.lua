return {
  "stevearc/conform.nvim",
  event = { "BufWritePre" },
  cmd = { "ConformInfo", "Format", "FormatDisable", "FormatEnable" },
  opts = {
    formatters_by_ft = {
      lua = { "stylua" },
      javascript = { { "prettierd", "prettier" } },
      typescript = { { "prettierd", "prettier" } },
      css = { { "prettierd", "prettier" } },
      scss = { { "prettierd", "prettier" } },
      less = { { "prettierd", "prettier" } },
      html = { { "prettierd", "prettier" } },
      json = { { "prettierd", "prettier" } },
      yaml = { { "prettierd", "prettier" } },
      toml = { { "prettierd", "prettier" } },
      rust = { "rustfmt" },
      sql = { "pg_format" },
      mysql = { "pg_format" },
      plsql = { "pg_format" },
      elixir = { "mix" },
    },
    format_on_save = function(bufnr)
      -- Disable autoformat on certain filetypes
      -- local ignore_filetypes = { "python" }
      -- if vim.tbl_contains(ignore_filetypes, vim.bo[bufnr].filetype) then
      -- 	return
      -- end
      -- Disable with a global or buffer-local variable
      if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
        return
      end
      -- Disable autoformat for files in a certain path
      local bufname = vim.api.nvim_buf_get_name(bufnr)
      if bufname:match("/euronetics/schedule/") or bufname:match("/euronetics/vbanken/") then
        return
      end
      if bufname:match("/code/jonashietala/templates") then
        return
      end
      if vim.bo[bufnr].filetype == "elixir" then
        return { timeout_ms = 500, lsp_fallback = false }
      end
      return { timeout_ms = 500, lsp_fallback = true }
    end,
  },
  init = function()
    local cmd = require("util.helpers").create_cmd
    local autocmd = vim.api.nvim_create_autocmd
    local augroup = vim.api.nvim_create_augroup

    cmd("Format", function(args)
      local range = nil
      if args.count ~= -1 then
        local end_line = vim.api.nvim_buf_get_lines(0, args.line2 - 1, args.line2, true)[1]
        range = {
          start = { args.line1, 0 },
          ["end"] = { args.line2, end_line:len() },
        }
      end
      require("conform").format({ async = true, lsp_fallback = true, range = range })
    end, { range = true })

    cmd("FormatDisable", function(args)
      if args.bang then
        -- FormatDisable! will disable formatting just for this buffer
        vim.b.disable_autoformat = true
      else
        vim.g.disable_autoformat = true
      end
    end, {
      desc = "Disable autoformat-on-save",
      bang = true,
    })
    cmd("FormatEnable", function()
      vim.g.disable_autoformat = false
    end, {
      desc = "Re-enable autoformat-on-save",
    })

    -- Didn't manage to add formatting specified with a lua function
    -- to conform, so do it manually.
    autocmd("BufWritePre", {
      pattern = "*.scm",
      group = augroup("scm", { clear = true }),
      callback = require("custom/format_queries").format,
    })
  end,
}
