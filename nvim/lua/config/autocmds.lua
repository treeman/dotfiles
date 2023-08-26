local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup

autocmd('TextYankPost', {
  callback = function() vim.highlight.on_yank() end,
  desc = "Briefly highlight yanked text",
  group = augroup('yank', { clear = true })
})

local filegroup = augroup('filegroup', { clear = true })
autocmd('FileType', {
  pattern = "html",
  group = filegroup,
  command = 'setlocal ts=2 sts=2 sw=2 expandtab'
})
autocmd('FileType', {
  pattern = "javascript",
  group = filegroup,
  command = 'setlocal ts=2 sts=2 sw=2'
})
autocmd('FileType', {
  pattern = "json",
  group = filegroup,
  command = 'setlocal ts=2 sts=2 sw=2 expandtab'
})

autocmd('FileType', {
  pattern = "lua",
  group = filegroup,
  command = 'setlocal ts=2 sts=2 sw=2'
})
