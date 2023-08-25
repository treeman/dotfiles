" Installation {{{
" Update plugins:
"   :PlugUpdate
"
" Install all available treesitter parsers:
"   :TSInstall all
"
" Dependencies:
"   git, ripgrep, fd, bat, go, trash-cli (for trashing via Fern)
"   pip3 install --upgrade pynvim (but I try not to use any python plugins...)
"   go get github.com/mattn/efm-langserver
"
" Formating
"   prettier (many languages)   sudo npm install -g prettier
"   lua                         cargo install stylua
"
" LSP servers:
"   rust-analyzer:
"     https://github.com/rust-analyzer/rust-analyzer
"     cargo xtask install --server
"
"   elixir-ls
"
"   Others via nvim-lsp-install:
"     :LspInstall <server>
" }}}
" Basic {{{
" Difficult to use fish as a default shell as plugins may depend on POSIX
" Instead launch terminal with fish
set shell=/bin/bash

" Enable mouse
set mouse=a
" Use CLIPBOARD register + as default
" Remember to install "xsel" for this to work!
" For Neovim in WSL see:
" https://github.com/neovim/neovim/wiki/FAQ#how-to-use-the-windows-clipboard-from-wsl
set clipboard+=unnamedplus

" Kill annoying beep sound
set visualbell

" Files
set backupdir=~/.config/nvim/backup " where to put backup
set backup " make backup files
set noswapfile " just annoying when I forcefully kill vim with the recovery
set directory=~/.config/nvim/tmp,~/tmp,/tmp " store swaps here if we do enable it
set wildignore=*.swp,*.bak,*.pyc,*.class,*.o,*.obj,*.ali " ignore files for file handling
set hidden " Can change buffers without saving

" Shada/viminfo
" Also used to store marks to quickly edit files:
"   C     cheat sheet
"   V     vimrc
set shada=!,'1000,<100,s100,h,f1
set shadafile=~/.config/nvim/.shada

" Interactivity
set hlsearch " highlight search terms
set incsearch " show search mathes as you type
set inccommand=split " live update for subtitute commands
let g:highlightedyank_highlight_duration = 500 " shorter highlighting for yank
set lazyredraw " speed up macros

" Text display
set list " show tabs
set listchars=tab:>-,trail:- " show tabs and trailing spaces

" Text formatting
set expandtab " no real tabs please!
set shiftround " when at 3 spaces, and I hit > ... go to 4, not 5
set shiftwidth=4 " auto indent amount when using indents ex >> and <<
set softtabstop=4 " when hitting tab or backspace, how wide should a tab be
set tabstop=4 " tabs width
set autoindent " keep indenting after newline
set smarttab " insert tabs on the start according to shiftwidth, not tabstop

" UI
set relativenumber " display relative line numbers
set number " show line numbers
set statusline=%<%{FugitiveStatusline()}%=%t\ %m%r%h%w%y%=%c%V,\ %l/%L\ %a\ 0x%0B\ %p%%
set laststatus=2 " always show the status line
set linespace=0 " don't insert any extra pixel lines between rows
set report=0 " tell us when anything is changed via :...
set shortmess=aOstTc " shortens messages to aviod 'perss a key' prompt
set ruler " always show current positions along the bottom
set showcmd " show the command being typed
set signcolumn=yes " Use a gutter for git-gutter and LSP messages
set completeopt=menuone,noselect " Required settings for nvim-cmp

augroup CursorLineOnlyInActiveWindow
  autocmd!
  autocmd VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  autocmd WinLeave * setlocal nocursorline
augroup END

" Use gx to open urls in firefox
let g:netrw_browsex_viewer = "firefox"

" Must set leaders before plugins
let mapleader = " "
let maplocalleader = "-"

" }}}
" Plugins {{{

filetype plugin indent on

" See :Plug* for commands
call plug#begin('~/.config/nvim/plugged')
" Colorscheme
Plug 'https://github.com/ellisonleao/gruvbox.nvim'
Plug 'https://github.com/sainnhe/gruvbox-material'
Plug 'https://github.com/rebelot/kanagawa.nvim'
" Excellent fuzzy finder
Plug 'https://github.com/junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'https://github.com/junegunn/fzf.vim'
" Another fuzzy finder
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build' }
Plug 'https://github.com/gbrlsnchs/telescope-lsp-handlers.nvim'
Plug 'nvim-telescope/telescope-file-browser.nvim'
" Personal wiki
Plug 'https://github.com/vimwiki/vimwiki.git', { 'branch': 'dev' }
Plug 'nvim-neorg/neorg', { 'do': ':Neorg sync-parsers' }
" Avoid mistyping filenames, ask which file to open if file not find
Plug 'https://github.com/EinfachToll/DidYouMean.git'
" Easy way to comment things
Plug 'https://github.com/tomtom/tcomment_vim'
" For many more features see tcomment_vim
" This is also an option
" Plug 'https://github.com/b3nj5m1n/kommentary', { 'branch': 'main' }
" Enhance the '.' operator
Plug 'https://github.com/tpope/vim-repeat'
" Easily surround things
Plug 'https://github.com/tpope/vim-surround'
" More targets
Plug 'https://github.com/wellle/targets.vim'
" Highlight what was yanked
Plug 'https://github.com/machakann/vim-highlightedyank'
" Vim cheat sheet
" Plug 'https://github.com/lifepillar/vim-cheat40'
" Telescope powered cheat sheet
Plug 'sudormrfbin/cheatsheet.nvim'
" Generic lua plugins
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'rktjmp/lush.nvim'
" Indentation lines for visual aid
Plug 'https://github.com/lukas-reineke/indent-blankline.nvim'
" Make * and # search visually
Plug 'https://github.com/nelstrom/vim-visual-star-search'
" Use s as a two-char f
Plug 'https://github.com/justinmk/vim-sneak'
" nvim in Firefox
" Peek registry contents, for easy use of " and @
Plug 'https://github.com/junegunn/vim-peekaboo'
" Display colors
Plug 'rrethy/vim-hexokinase', { 'do': 'make hexokinase' }
" Sudo write for neovim
Plug 'https://github.com/lambdalisue/suda.vim'
" Kill buffers smartly
Plug 'mhinz/vim-sayonara', { 'on': 'Sayonara' }
" Scratch buffer
" Plug 'https://github.com/idbrii/itchy.vim'
" Automatically insert end in insert mode for some languages
Plug 'https://github.com/tpope/vim-endwise'
" Light statusbar
Plug 'https://github.com/itchyny/lightline.vim'
Plug 'shinchu/lightline-gruvbox.vim'
" Treesitter
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'nvim-treesitter/playground'
Plug 'RRethy/nvim-treesitter-textsubjects'
" Interactively swap parameters
Plug 'mizlan/iswap.nvim'
" Dispatch async things
Plug 'https://github.com/tpope/vim-dispatch'
" Maximize for windows
Plug 'https://github.com/szw/vim-maximizer'
" Autocompletion framework with a ton of support
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/nvim-cmp'
Plug 'https://github.com/f3fora/cmp-spell'
Plug 'https://github.com/hrsh7th/cmp-calc'
Plug 'https://github.com/hrsh7th/cmp-path'
Plug 'https://github.com/hrsh7th/cmp-omni'
Plug 'crispgm/cmp-beancount'
" Align text around
Plug 'junegunn/vim-easy-align'
" Repalce text inside quickfix
Plug 'gabrielpoca/replacer.nvim'
" Markdown previewer
Plug 'ellisonleao/glow.nvim', {'do': ':GlowInstall', 'branch': 'main'}
" zoxide, a smart cd
Plug 'https://github.com/nanotee/zoxide.vim'
Plug 'https://github.com/jvgrootveld/telescope-zoxide'
" Show diagnostics and stuff
Plug 'nvim-tree/nvim-web-devicons'
Plug 'https://github.com/folke/trouble.nvim'

" File explorer
Plug 'https://github.com/lambdalisue/fern.vim', {'branch': 'main'}
Plug 'https://github.com/lambdalisue/nerdfont.vim'
Plug 'https://github.com/lambdalisue/fern-renderer-nerdfont.vim'
Plug 'https://github.com/lambdalisue/fern-git-status.vim'
Plug 'https://github.com/lambdalisue/glyph-palette.vim'
" Fixes some performance issues
Plug 'antoinemadec/FixCursorHold.nvim'

" Specific file support
Plug 'https://github.com/nathangrigg/vim-beancount'
Plug 'https://github.com/dag/vim-fish.git'
Plug 'https://github.com/rust-lang/rust.vim'
Plug 'https://github.com/cespare/vim-toml.git'
Plug 'https://github.com/hail2u/vim-css3-syntax.git'
Plug 'https://github.com/wlangstroth/vim-racket'
Plug 'https://github.com/otherjoel/vim-pollen.git'
Plug 'elixir-tools/elixir-tools.nvim'
Plug 'https://github.com/elixir-editors/vim-elixir.git'
Plug 'simrat39/rust-tools.nvim'
Plug 'mhinz/vim-mix-format'
" Autoformat for different languages
Plug 'sbdchd/neoformat'

" Debugger
Plug 'mfussenegger/nvim-dap'

" LSP support
" See doc :help lsp
" Collection of common configurations for the Nvim LSP client
Plug 'neovim/nvim-lspconfig'
" Help to install language servers
Plug 'williamboman/mason.nvim', { 'do': ':MasonUpdate' }
Plug 'williamboman/mason-lspconfig.nvim'
" Extensions to built-in LSP, for example, providing type inlay hints
Plug 'tjdevries/lsp_extensions.nvim'
Plug 'https://github.com/onsails/lspkind-nvim'
Plug 'kosayoda/nvim-lightbulb'
" Automatically change root to current file
Plug 'ahmedkhalf/lsp-rooter.nvim'

" Snippets
Plug 'hrsh7th/cmp-vsnip'
Plug 'hrsh7th/vim-vsnip'
"Plug 'hrsh7th/vim-vsnip-integ'

" Git plugins
Plug 'lewis6991/gitsigns.nvim'
Plug 'https://github.com/tpope/vim-fugitive'
Plug 'https://github.com/rbong/vim-flog'
Plug 'https://github.com/rhysd/git-messenger.vim'

" Stop the jjjj spam kthx
" Plug 'takac/vim-hardtime'
call plug#end()

" }}}
" Appearance {{{

syntax enable

let g:gruvbox_contrast_dark = "hard"
let g:gruvbox_contrast_light = "soft"
if $TERM == "xterm-kitty" || $TERM == "alacritty" || $TERM == "xterm-256color"
  set termguicolors
endif
set background=dark
let g:gruvbox_italic = 1
let g:gruvbox_bold = 1
let g:gruvbox_italicize_comments = 1
" Need to override these
" :hi @text.strong gui=bold
" :hi @text.emphasis gui=italic
" But bold isn't shown either way?

let g:gruvbox_flat_style = "hard"

let g:gruvbox_material_background = 'hard'
" let g:gruvbox_material_better_performance = 1
" colorscheme gruvbox-material

" To avoid the low contrast gray on 2nd left side
"
let g:lightline_gruvbox_style = 'hard_left'
" let g:lightline = {'colorscheme': 'gruvbox-flat'}

" let g:tokyonight_style = "night"


set guifont=Iosevka\ Custom\ Medium:h12
let g:neovide_cursor_animation_length = 0.13
let g:neovide_transparency = 1.0
let g:neovide_window_floating_opacity = 1.0
let g:neovide_floating_opacity = 1.0
let g:neovide_floating_blur = 1.0

" For more info see:
" :h statusline
" :h g:lightline.component
let g:lightline = {
  \ 'colorscheme': 'gruvbox',
  \ 'active': {
  \   'left': [ [ 'mode', 'paste' ],
  \             [ 'gitbranch', 'gitstatus', 'readonly', 'modified' ],
  \             [ 'filename' ] ],
  \   'right': [ [ 'lineinfo' ],
  \              [ 'charvaluehex' ],
  \              [ 'lsp', 'spell', 'fileformat', 'fileencoding', 'filetype' ] ]
  \ },
  \ 'inactive': {
  \   'left': [ [ 'filename' ] ],
  \   'right': [ [ 'lineinfo' ] ]
  \ },
  \ 'component': {
  \   'charvaluehex': '0x%0B',
  \   'lineinfo': '%c:%l/%L%<'
  \ },
  \ 'component_function': {
  \   'gitbranch': 'FugitiveHead',
  \   'gitstatus': 'LightlineGitStatus',
  \   'spell': 'LightlineSpell',
  \   'filename': 'LightlineFilename',
  \   'fileformat': 'LightlineFileformat',
  \   'filetype': 'LightlineFiletype',
  \   'fileencoding': 'LightlineFileEncoding',
  \   'lsp': 'LightlineLSP',
  \ },
  \ 'mode_map': {
  \   'n' : 'N',
  \   'i' : 'I',
  \   'R' : 'R',
  \   'v' : 'V',
  \   'V' : 'VL',
  \   "\<C-v>": 'VB',
  \   'c' : 'C',
  \   's' : 'S',
  \   'S' : 'SL',
  \   "\<C-s>": 'SB',
  \   't': 'T',
  \ },
\ }

" These functions truncates away a bunch of stuff when width is smaller
function! LightlineGitStatus()
  if winwidth(0) <= 95
    return ""
  endif

  return get(b:,'gitsigns_status','')
endfunction

function! LightlineFilename()
  return expand('%:t')
  "return winwidth(0) > 110 ? expand('%:f') : expand('%:t')
endfunction

function! LightlineSpell()
  return winwidth(0) > 70 ? (&spell ? &spelllang : '') : ''
endfunction

function! LightlineFileformat()
  return winwidth(0) > 95 ? &fileformat : ''
endfunction

function! LightlineFiletype()
  return winwidth(0) > 95 ? (&filetype !=# '' ? &filetype : 'no ft') : ''
endfunction

function! LightlineFileEncoding()
  return winwidth(0) > 95 ? (&fenc !=# '' ? &fenc : &enc) : ''
endfunction

function! LightlineLSP()
  let no_lsp = luaeval('next(vim.lsp.buf_get_clients()) == nil')
  if no_lsp
    return ''
  else
    return 'LSP'
  endif
endfunction

" Workaround for https://github.com/lukas-reineke/indent-blankline.nvim/issues/59
set colorcolumn=99999

" This is too verbose unfortunately
" function! LightlineTreesitter()
"   return winwidth(0) > 70 ? nvim_treesitter#statusline(40) : ''
" endfunction
" }}}
" Mapping {{{

" Reload vimrc
nnoremap <leader>sv :so $MYVIMRC<CR>

" Edit file with prefilled path from the current file
nnoremap <leader>ef :e <C-R>=expand('%:p:h') . '/'<CR>
" Allow saving of files as sudo when I forgot to start vim using sudo.
"cmap w!! w !sudo tee > /dev/null %
"com -bar W exe 'w !sudo tee >/dev/null %:p:S' | setl nomod
cmap w!! SudaWrite
" Open scratch file
nmap <leader>ss :e ~/norg/scratch.norg<CR>

" Find files
nnoremap <silent> <leader>f :Telescope find_files<CR>
" Find files relative to current file
nnoremap <silent> <leader>F :lua require('telescope.builtin').find_files( { cwd = vim.fn.expand('%:p:h') })<CR>
" Find in files
nnoremap <silent> <leader>/ :execute 'Rg ' . input('Rg/')<CR>
" Would be nice to use live updating grep, but it feels very slow as it blocks.
" Tracking issue: https://github.com/nvim-telescope/telescope.nvim/issues/392
"nnoremap <silent> <leader>/ :Telescope live_grep<CR>
" Find from open buffers
" nnoremap <silent> <leader>b :Telescope buffers<CR>
nnoremap <silent> <leader>b :lua require('telescope_extra').my_buffer()<CR>
" File drawer
nnoremap <leader>d :Fern . -drawer -toggle<CR>

" Notes and vimwiki editing
" nnoremap <leader>w :lua require('telescope.builtin').find_files( { cwd = vim.fn.expand('~/vimwiki') })<CR>
" nnoremap <leader>ew :e <C-R>=expand('~/vimwiki/')<CR>
nnoremap <leader>ej :call v:lua.weekly_journal()<CR>

nnoremap <leader>n :lua require('telescope_extra').open_norg('')<CR>
nnoremap <leader>ep :lua require('telescope_extra').open_norg('projects')<CR>
nnoremap <leader>ea :lua require('telescope_extra').open_norg('areas')<CR>
nnoremap <leader>er :lua require('telescope_extra').open_norg('resources')<CR>
nnoremap <leader>eA :lua require('telescope_extra').open_norg('archive')<CR>

" Use a weekly journal instead.
" nnoremap <leader>j :Neorg journal today<CR>
" nnoremap <leader>J :Neorg journal yesterday<CR>

" CTRL-A CTRL-Q to select all and build quickfix list
function! s:build_quickfix_list(lines)
  call setqflist(map(copy(a:lines), '{ "filename": v:val }'))
  copen
  cc
endfunction

let g:fzf_action = {
  \ 'ctrl-q': function('s:build_quickfix_list'),
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }

let $FZF_DEFAULT_OPTS = '--bind ctrl-a:select-all'

" Copy/paste to mouse clipboard quickly
nnoremap <silent> <leader>p "*p
nnoremap <silent> <leader>P "*P
nnoremap <silent> <leader>y "*y
nnoremap <silent> <leader>Y "*Y
" Don't overwrite register when pasting in visual selection
xnoremap p "_dP

" Special chars
"inoremap <C-l> λ
"inoremap <C-e> ◊
"inoremap <C-v>l λ
"inoremap <C-v>e ◊

" Easy way to launch terminal
" function! FishTerm()
"   execute 'e term://fish'
"   " Make escape enter normal mode in terminal as well
"   tnoremap <buffer> <Esc> <C-\><C-n>
"   tnoremap <buffer> <C-v><Esc> <Esc>
"   setlocal nonumber
"   setlocal norelativenumber
" endfunction
" nnoremap <leader>tt :call FishTerm()<CR>
" nnoremap <leader>tv :vs <bar> :call FishTerm()<CR>
" nnoremap <leader>ts :sp <bar> :call FishTerm()<CR>

" Use ( as a toggle prefix everywhere
nmap ( [
nmap ) ]
omap ( [
omap ) ]
xmap ( [
xmap ) ]

" Use hjkl on split keyboard instead!
noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Left> <Nop>
noremap <Right> <Nop>

" Happy window switching
if $NORMAL_KEYBOARD == 1
  tnoremap <C-h> <c-\><c-n><c-w>h
  tnoremap <C-j> <c-\><c-n><c-w>j
  tnoremap <C-k> <c-\><c-n><c-w>k
  tnoremap <C-l> <c-\><c-n><c-w>l

  nnoremap <C-h> <c-w>h
  nnoremap <C-j> <c-w>j
  nnoremap <C-k> <c-w>k
  nnoremap <C-l> <c-w>l

  " Clear screen and turn off search highlighting until the next time we search
  nnoremap <silent> <M-l> :<C-u>nohlsearch<CR><C-l>
else
  tnoremap <C-left> <c-w>h
  tnoremap <C-down> <c-w>j
  tnoremap <C-up> <c-w>k
  tnoremap <C-right> <c-w>l

  nnoremap <C-left> <c-w>h
  nnoremap <C-down> <c-w>j
  nnoremap <C-up> <c-w>k
  nnoremap <C-right> <c-w>l

  nnoremap <C-l> :<C-u>nohlsearch<CR><C-l>
end

" Create splits with <C-w>v and <C-w>s, or :sp and :vs
" Close the current buffer if it's not shown in another window, but keep the window itself
nnoremap <leader>Q :Sayonara!<CR>

let g:maximizer_set_default_mapping = 0
nnoremap <silent><C-w>m :MaximizerToggle<CR>

" Replace things in quickfix
nmap <leader>r :lua require("replacer").run()<cr>

" Goto previous buffer
nnoremap <leader>B :edit #<CR>

" Swap parameters (if treesitter exists)
nnoremap <leader>S :ISwapWith<CR>

" Snippets
" Expand or jump
imap <expr> <C-t> vsnip#available(1) ? '<Plug>(vsnip-expand-or-jump)' : '<C-t>'
smap <expr> <C-t> vsnip#available(1) ? '<Plug>(vsnip-expand-or-jump)' : '<C-t>'
" Jump backward
imap <expr> <C-s> vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)'      : '<C-s>'
smap <expr> <C-s> vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)'      : '<C-s>'
" Select text for snippets
xmap <C-y> <Plug>(vsnip-select-text)
"xmap yv <Plug>(vsnip-cut-text)

" Trim whitespaces
" FIXME Should create a map for it to trim only the selection
nnoremap <leader>tw :silent! %s/\s\+$//<CR>:retab<CR>

" Supercharged spell correction!
nnoremap <silent> z= :Telescope spell_suggest<CR>

" nnoremap <silent> <leader>q :Telescope quickfix<CR>
" nnoremap <silent> <leader>l :Telescope loclist<CR>
nnoremap <silent> <leader>q :TroubleToggle quickfix<CR>
nnoremap <silent> <leader>l :TroubleToggle loclist<CR>

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" Remaps to try from theprimaegen
" Move visual selection up/down
vnoremap <C-j> :m '>+1<CR>gv=gv
vnoremap <C-k> :m '<-2<CR>gv=gv
" Don't move cursor when joining lines
noremap J mzJ`z
" Keep cursor in the middle when paging
noremap <C-d> <C-d>zz
noremap <C-u> <C-u>zz
noremap <PageUp> <PageUp>zz
noremap <PageDown> <PageDown>zz
" Keep search terms in the midlle
noremap n nzzzv
noremap N Nzzzv

" }}}
" Commands {{{
" Insert datetime
command! InsertDateTime :normal i<C-R>=system("date +%FT%T%:z")<CR>
" Insert a uuid
command! InsertUUID :normal i<C-R>=system('uuidgen')<CR>

" }}}
" Functions {{{

" Counts the number of # in markdown headers
function! MarkdownLevel()
  let h = matchstr(getline(v:lnum), '^#\+')
  if empty(h)
    return "="
  else
    return ">" . len(h)
  endif
endfunction

" }}}
" Lua configs {{{
lua require("lsp_config")
lua require("treesitter_config")
lua require("telescope")
lua require("telescope_config")
lua require("generic")
lua require("workflow")
" }}}
" File specific {{{
" Vimwiki {{{

" Block vimwiki from hijacking markdown files
let g:vimwiki_global_ext = 0
" Set markdown as default syntax
let g:vimwiki_list = [{'path': '~/vimwiki/',
                      \ 'syntax': 'markdown', 'ext': '.markdown'}]

let g:vimwiki_folding = 'custom'

" Vimwiki registers tons of keybindings. I don't use the vast majority,
" and some override my own, so do things manually instead.
let g:vimwiki_key_mappings = { 'all_maps': 0 }

augroup vimwikigroup
  autocmd!
  autocmd FileType vimwiki setlocal foldmethod=expr |
    \ setlocal foldenable | set foldexpr=MarkdownLevel()

  autocmd FileType vimwiki nnoremap <buffer> <CR> :VimwikiFollowLink<CR>
  autocmd FileType vimwiki nnoremap <buffer> <Backspace> :VimwikiGoBackLink<CR>
  autocmd FileType vimwiki nnoremap <buffer> <Tab> :VimwikiNextLink<CR>
  autocmd FileType vimwiki nnoremap <buffer> <S-Tab> :VimwikiPrevLink<CR>
  autocmd FileType vimwiki nnoremap <buffer> <C-Space> :VimwikiToggleListItem<CR>
augroup END

" }}}
" Markdown {{{

augroup markdowngroup
  autocmd!
  autocmd FileType markdown setlocal foldexpr=MarkdownLevel()
  autocmd FileType markdown setlocal foldmethod=expr
  autocmd FileType markdown :normal zR
augroup END

" }}}
" Beancount {{{

augroup beancountgroup
  autocmd!
  autocmd FileType beancount setlocal foldexpr=MarkdownLevel()
  autocmd FileType beancount setlocal foldmethod=expr
  " Weirdly enough, this is too slow...
  " autocmd BufWritePre *.beancount Neoformat
  " autocmd FileType beancount lua require'cmp'.setup.buffer {
  " \   sources = {
  " \     { name = 'calc' },
  " \     { name = 'omni' },
  " \     { name = 'spell' },
  " \     { name = 'buffer' },
  " \   },
  " \ }
augroup END

" }}}
" Pollen {{{

augroup pollengroup
  autocmd!
  au! BufRead,BufNewFile *.p set filetype=pollen
  au! BufRead,BufNewFile *.pm set filetype=pollen
  au! BufRead,BufNewFile *.pmd set filetype=pollen
  au! BufRead,BufNewFile *.pp set filetype=pollen
  au! BufRead,BufNewFile *.ptree set filetype=pollen
  " Soft wrap (don't affect buffer)
  autocmd FileType pollen setlocal wrap
  " Wrap on word-breaks only
  autocmd FileType pollen setlocal linebreak
  autocmd FileType pollen setlocal ts=2 sts=2 sw=2

  autocmd FileType pollen inoremap <C-l> λ
  autocmd FileType pollen inoremap <C-e> ◊
augroup END

augroup racket
  autocmd!
  autocmd FileType pollen inoremap <C-l> λ
  autocmd FileType pollen inoremap <C-e> ◊
augroup END

" }}}
" Rust {{{
" let g:neoformat_python_rustfmt = {
"             \ 'args': ['--edition 2021'],
"             \ }
" let g:neoformat_enabled_rust = ['rustfmt']

augroup rustgroup
  autocmd!
  autocmd BufWritePre *.rs Neoformat
  autocmd Filetype rust nnoremap <leader>c :Dispatch cargo clippy --all-targets --all-features -- -D warnings<CR>
  autocmd FileType rust let b:dispatch = 'cargo check'
augroup END
" }}}
" Elixir {{{
let g:mix_format_on_save = 1
let g:mix_format_silent_errors = 1
augroup elixir_group
  autocmd!
  autocmd FileType elixir normal zR
augroup END

" let g:neoformat_verbose = 1

augroup eelixir_group
  autocmd!
  autocmd BufWritePost *.html.heex silent :!mix format %
  " autoread doesn't work, so this is a workaround
  autocmd BufWritePost *.html.heex silent :e
augroup END
" }}}
" Web {{{

augroup web
  autocmd!
  autocmd Filetype html setlocal ts=2 sts=2 sw=2 expandtab
  autocmd Filetype javascript setlocal ts=2 sts=2 sw=2
  autocmd Filetype json setlocal ts=2 sts=2 sw=2 noexpandtab
  autocmd FileType typescriptreact let b:dispatch = 'yarn next build'
  autocmd BufWritePre *.json,*.js,*.ts,*.tsx,*.css,*.html,*.scss Neoformat
augroup END

" }}}
" Python {{{

augroup python
  autocmd!
  autocmd BufWritePre *.py Neoformat
augroup END

" }}}
" Vim {{{

augroup vim_filetype
  autocmd!
  autocmd Filetype vim setlocal foldmethod=marker
  autocmd Filetype vim setlocal nofoldenable
  autocmd Filetype vim setlocal ts=2 sts=2 sw=2
  "autocmd FileType vim :normal zM
augroup END

" }}}
"Firenvim {{{
if exists('g:started_by_firenvim') && g:started_by_firenvim
  let g:firenvim_config = { 
      \ 'localSettings': {
          \ '.*': {
              \ 'cmdline': 'firenvim',
              \ 'priority': 0,
              \ 'selector': 'textarea',
              \ 'takeover': 'never',
          \ },
      \ }
  \ }

  set laststatus=0 nonumber noruler noshowcmd
  set background=light

  augroup firenvim
    autocmd!
    autocmd BufEnter *.txt setlocal filetype=markdown
  augroup END
endif
"}}}
" Lua {{{
augroup luagroup
  autocmd!
  autocmd Filetype lua setlocal ts=2 sts=2 sw=2
  autocmd BufWritePre *.lua Neoformat
augroup END
"}}}
"Git {{{
nnoremap gs :Git<CR>
nnoremap g<space> :Git 
nnoremap gll :Flogsplit<CR>
nnoremap glf :Flogsplit -path=%<CR>
xnoremap glf :Flogsplit -- --no-patch<CR>
nnoremap <silent> <leader>C :Commits<CR>
nnoremap gb :Telescope git_branches<CR>

" Fugitive Conflict Resolution
nnoremap gds :Gdiffsplit!<CR>
nnoremap gdh :diffget //2<CR>
nnoremap gdl :diffget //3<CR>

" let g:git_messenger_no_default_mappings = v:true
" nnoremap g? <Plug>(git-messenger)

" Toggle --no-patch for all commits.
" Toggling for specific commits isn't supported, but maybe will be later.
" https://github.com/rbong/vim-flog/issues/39
function ToggleFlogNoPatch() abort
    let l:state = flog#get_state()
    let l:raw_args = l:state['raw_args']
    if empty(l:raw_args)
        let l:raw_args = ''
    endif
    let l:split_raw_args = split(l:raw_args)
    let l:no_patch_arg_index = index(l:split_raw_args, '--no-patch')
    if l:no_patch_arg_index >= 0
        if len(l:split_raw_args) == 1
            let l:raw_args = v:null
        else
            call remove(l:split_raw_args, l:no_patch_arg_index)
            let l:raw_args = join(l:split_raw_args, ' ')
        endif
    else
        let l:raw_args .= ' --no-patch'
    endif
    let l:state['raw_args'] = l:raw_args
    call flog#populate_graph_buffer()
endfunction
autocmd FileType floggraph nnoremap <buffer> <silent> = :<C-U>call ToggleFlogNoPatch()<CR>

"}}}
"{{{ cheat40
" Don't use default cheat sheet
let g:cheat40_use_default = 0
" Hide folds
let g:cheat40_foldlevel = 0

function! EditCheat40()
  " Taken from cheat40#open in vim-cheat40
  setlocal foldmethod=marker foldtext=substitute(getline(v:foldstart),'\\s\\+{.*$','','')
  execute 'setlocal foldlevel='.get(g:, 'cheat40_foldlevel', 1)
  setlocal concealcursor=nc conceallevel=3
  setlocal expandtab nospell nowrap textwidth=40
  setlocal fileencoding=utf-8 filetype=cheat40
  setlocal iskeyword=@,48-57,-,/,.,192-255
endfunction

augroup cheatgroup
  autocmd!
  au! BufRead,BufNewFile cheat40.txt call EditCheat40()
augroup END
"}}}
"{{{ hexokinase
let g:Hexokinase_highlighters = [ 'virtual' ]
" Filetypes to match
" Because it's async maybe this is completely unnecessary...
let g:Hexokinase_ftEnabled = ['css', 'html', 'javascript', 'scss', 'markdown', 'vimwiki', 'json', 'lua']
" Patterns to match
let g:Hexokinase_optInPatterns = 'full_hex,triple_hex,rgb,rgba,hsl,hsla'
" Filetype specific patterns to match
let g:Hexokinase_ftOptInPatterns = {
\     'css': 'full_hex,triple_hex,rgb,rgba,hsl,hsla,colour_names',
\     'scss': 'full_hex,triple_hex,rgb,rgba,hsl,hsla,colour_names',
\     'html': 'full_hex,triple_hex,rgb,rgba,hsl,hsla,colour_names'
\ }
"}}}
"{{{ fern
let g:fern#renderer = "nerdfont"

function! s:init_fern() abort
  " Expand and collapse with arrows
  nmap <buffer><nowait> <right> <Plug>(fern-action-expand)
  nmap <buffer><nowait> <left> <Plug>(fern-action-collapse)

  " Don't overwrite window switching
  nnoremap <buffer><nowait> <C-h> <c-w>h
  nnoremap <buffer><nowait> <C-l> <c-w>l
  nnoremap <buffer><nowait><silent> <M-l><Plug>(fern-action-redraw)
endfunction

augroup my-fern
  autocmd! *
  autocmd FileType fern call s:init_fern()
  autocmd FileType fern call glyph_palette#apply()
augroup END

"}}}
" FZF {{{
" Customize fzf colors to match your color scheme
" - fzf#wrap translates this to a set of `--color` options
let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'border':  ['fg', 'Ignore'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }

" Default fzf layout
" - Popup window
let g:fzf_layout = { 'window': '-tabnew' }
" }}}
" Sneak {{{
" Use seank as a minimalist alternative to EasyMotion
"let g:sneak#label = 1
" }}}
" Hard time {{{
" On for every buffer
let g:hardtime_default_on = 1
let g:list_of_normal_keys = ["h", "j", "k", "l"]
let g:list_of_visual_keys = ["h", "j", "k", "l"]
let g:list_of_insert_keys = ["<UP>", "<DOWN>", "<LEFT>", "<RIGHT>"]
let g:list_of_disabled_keys = []
let g:hardtime_showmsg = 1
" Allow "jh" but not "jj"
let g:hardtime_allow_different_key = 1
" Allow "jj" but not "jjj"
let g:hardtime_maxcount = 2
" }}}
" vim:set sw=2 sts=2:
