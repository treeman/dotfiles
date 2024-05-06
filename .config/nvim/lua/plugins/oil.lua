local opts = {
  default_file_explorer = true,
  keymaps = {
    ["<BS>"] = "actions.parent",
    ["!"] = "actions.toggle_hidden",
  },
}

return {
  "stevearc/oil.nvim",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  opts = opts,
  command = "Oil",
  -- This is how to lazy load oil according to:
  -- https://github.com/folke/lazy.nvim/issues/533
  -- ... Overkill?
  -- Of course, I just wanted to know how you should do it.
  init = function()
    if vim.fn.argc() == 1 then
      local stat = vim.loop.fs_stat(vim.fn.argv(0))
      if stat and stat.type == "directory" then
        require("lazy").load({ plugins = { "oil.nvim" } })
      end
    end
    if not require("lazy.core.config").plugins["oil.nvim"]._.loaded then
      vim.api.nvim_create_autocmd("BufNew", {
        callback = function()
          if vim.fn.isdirectory(vim.fn.expand("<afile>")) == 1 then
            require("lazy").load({ plugins = { "oil.nvim" } })
            -- Once oil is loaded, we can delete this autocmd
            return true
          end
        end,
      })
    end
  end,
}
