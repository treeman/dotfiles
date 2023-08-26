-- Must set leaders before plugins are loaded
vim.g.mapleader = [[ ]]
vim.g.maplocalleader = [[_]]

-- Basic lazy.nvim setup as copied from the readme
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- Load plugins specs from lua/plugins/*.lua
require("lazy").setup("plugins", {
  defaults = {
    lazy = true,
  }
})

require("config.options")
require("config.autocmds")
require("config.keymaps")
require("config.colorscheme")


