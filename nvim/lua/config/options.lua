-- Difficult to use fish as a default shell as plugins may depend on POSIX
-- Instead launch terminal with fish
vim.opt.shell = "/bin/bash"

-- Use CLIPBOARD register + as default
-- Remember to install "xsel" for this to work!
-- For Neovim in WSL see:
-- https://github.com/neovim/neovim/wiki/FAQ#how-to-use-the-windows-clipboard-from-wsl
vim.opt.clipboard:append { 'unnamed', 'unnamedplus' }

-- Backup files
vim.opt.backupdir = vim.fn.expand("~/.config/nvim/backup") --where to put backup
vim.opt.backup = true -- make backup files
vim.opt.swapfile = false -- just annoying when I forcefully kill vim with the recovery
vim.opt.wildignore = "*.swp,*.bak,*.pyc,*.class,*.o,*.obj,*.ali" --ignore files for file handling
vim.opt.hidden = true -- can change buffers without saving
-- History and stuff
vim.opt.shada = "!,'1000,<100,s100,h,f1"
vim.opt.shadafile = vim.fn.expand("~/.config/nvim/.shada")

-- Prevent <leader> from timing out
vim.opt.timeout = false
vim.opt.ttimeout = true

-- Text display
vim.opt.list = true -- show tabs
vim.opt.listchars = "tab:>-,trail:-" -- show tabs and trailing spaces

-- Text formatting
vim.opt.expandtab = true -- no real tabs please!
vim.opt.shiftround = true -- when at 3 spaces, and I hit > ... go to 4, not 5
vim.opt.shiftwidth= 4  -- auto indent amount when using indents ex >> and <<
vim.opt.softtabstop = 4 -- when hitting tab or backspace, how wide should a tab be
vim.opt.tabstop = 4 -- tabs width
vim.opt.autoindent = true -- keep indenting after newline
vim.opt.smarttab = true -- insert tabs on the start according to shiftwidth, not tabstop

-- UI
--vim.opt.statusline = "%<%{FugitiveStatusline()}%=%t %m%r%h%w%y%=%c%V, %l/%L %a 0x%0B %p%%"
vim.opt.relativenumber = true-- display relative line numbers
vim.opt.number = true-- show line numbers
vim.opt.laststatus = 2 -- always show the status line
vim.opt.linespace = 0 -- don't insert any extra pixel lines between rows
vim.opt.report = 0 -- tell us when anything is changed via :...
vim.opt.shortmess = "aOstTc" -- shortens messages to aviod 'press a key' prompt
vim.opt.ruler = true-- always show current positions along the bottom
vim.opt.showcmd = true-- show the command being typed
vim.opt.signcolumn = "yes" -- Use a gutter for git-gutter and LSP messages
vim.opt.completeopt = "menuone,noselect" -- Required settings for nvim-cmp

vim.opt.spell = true
vim.opt.spelllang = { "en_us" }

vim.opt.lazyredraw = true -- speed up macros
