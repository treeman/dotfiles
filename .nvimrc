" Future ideas and TODOs {{{

" Pretty format XML, JSON
" ??
"
" nvim-lsp for rust
" https://dev.to/drmason13/configure-neovim-for-rust-development-1fjn
"
" Maybe use an LSP settings handler?
" https://github.com/mattn/vim-lsp-settings
"
" Add back <leader>n (or similar) to turn search highlight off until next time
"
" Move out file specific into ftplugin
" Create an easily accessible cheat sheet
" }}}
" Basic {{{
" Difficult to use fish as a default shell as plugins may depend on POSIX
" Instead launch terminal with fish
set shell=/bin/bash

" Enable mouse
set mouse=a
" Use CLIPBOARD register + as default
" Remember to install "xsel" for this to work!
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

" Interactivity
set hlsearch " highlight search terms
set incsearch " show search mathes as you type
set inccommand=split " live update for subtitute commands
let g:highlightedyank_highlight_duration = 500 " shorter highlighting for yank

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
set statusline=%<%t%m%r%h%w%=%c%V,\ %l/%L\ %a\ 0x%0B\ %p%%
set laststatus=2 " always show the status line
set linespace=0 " don't insert any extra pixel lines between rows
set report=0 " tell us when anything is changed via :...
set shortmess=aOstT " shortens messages to aviod 'perss a key' prompt
set ruler " always show current positions along the bottom
set showcmd " show the command being typed

" }}}
" Plugins {{{

filetype plugin indent on

" See :Plug* for commands
call plug#begin('~/.config/nvim/plugged')
" Colorscheme
Plug 'https://github.com/treeman/gruvbox.git'
" Excellent fuzzy finder
Plug 'https://github.com/junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'https://github.com/junegunn/fzf.vim'
" Personal wiki
Plug 'https://github.com/vimwiki/vimwiki.git', { 'branch': 'dev' }
" Avoid mistyping filenames, ask which file to open if file not find
Plug 'https://github.com/EinfachToll/DidYouMean.git'
" Easy way to comment things
Plug 'https://github.com/scrooloose/nerdcommenter.git'
" Highlight what was yanked
Plug 'https://github.com/machakann/vim-highlightedyank'
" Vim cheat sheet
Plug 'https://github.com/lifepillar/vim-cheat40'
" These are just for highlighting specific files
Plug 'https://github.com/nathangrigg/vim-beancount'
Plug 'https://github.com/dag/vim-fish.git'
Plug 'https://github.com/rust-lang/rust.vim'
Plug 'https://github.com/cespare/vim-toml.git'
Plug 'https://github.com/hail2u/vim-css3-syntax.git'
Plug 'https://github.com/wlangstroth/vim-racket'
Plug 'https://github.com/otherjoel/vim-pollen.git'
Plug 'https://github.com/elixir-editors/vim-elixir.git'
" LSP support
" See doc :help lsp
Plug 'https://github.com/neovim/nvim-lsp'
" coc.vim, intellisense engine
Plug 'https://github.com/neoclide/coc.nvim', {'branch': 'release'}
" nvim in Firefox
Plug 'https://github.com/glacambre/firenvim', { 'do': { _ -> firenvim#install(0) } }
call plug#end()

" Plugins recommended by Practical vim:
" surround.vim
"
" More fuzzy funding examples:
"https://github.com/junegunn/fzf/wiki/Examples-(vim)
"use enter key, CTRL-T, CTRL-X or CTRL-V to open selected files in the current window, in new tabs, in horizontal splits, or in vertical splits respectively.

" Random unsorted plugins
"https://www.reddit.com/r/vim/comments/gbhvlo/what_am_i_missing_by_not_using_fzf/
"https://www.reddit.com/r/vim/comments/8riofp/airlinelightline_not_useful/
"https://www.reddit.com/r/vim/comments/775n4o/is_there_any_way_to_switch_statusbar_color_or/
"https://www.reddit.com/r/neovim/comments/gjz5cx/whats_a_plugin_that_does_something_you_didnt/
"https://www.reddit.com/r/vim/comments/gk53u1/just_discovered_ca_and_cx/
"https://www.reddit.com/r/vim/comments/gjz27p/whats_a_plugin_that_does_something_you_didnt/
"https://www.reddit.com/r/vim/comments/gib54k/anyone_has_a_copy_of_custom_vim_refactorings/
"https://www.reddit.com/r/vim/comments/a0q8dv/id_like_to_update_to_modern_vim_practices_what/
"https://www.reddit.com/r/neovim/comments/g5uo37/supercharge_your_vim_with_fzf_ripgrep/
"https://www.reddit.com/r/vim/comments/gfouey/markdown_mode_with_collapsible_blocks_and/
"https://stackoverflow.com/questions/13337618/how-to-use-customized-key-to-start-visual-block-selection-in-vim
"https://blog.usejournal.com/a-detailed-guide-to-writing-your-first-neovim-plugin-in-rust-a81604c606b1?gi=2c1f7e07ec18
"https://medium.com/@caleb89taylor/a-guide-to-modern-web-development-with-neo-vim-333f7efbf8e2
"https://github.com/Shougo/denite.nvim/blob/master/README.md
"https://dev.to/drmason13/configure-neovim-for-rust-development-1fjn
"https://kodi.wiki/view/Add-on:VimCasts
"https://github.com/lambdalisue/gina.vim/blob/master/README.md
"https://github.com/jreybert/vimagit/blob/master/README.md
"https://github.com/tpope/vim-dispatch/blob/master/README.markdown

" nvim-lsp for rust
"https://dev.to/drmason13/configure-neovim-for-rust-development-1fjn

" Plugins to check
" Comment stuff out
" https://github.com/tpope/vim-commentary
" Git
" https://github.com/tpope/vim-fugitive
" Surrounding things
" https://github.com/tpope/vim-surround
" Launch async things
" https://github.com/tpope/vim-dispatch
" Show git changes in gutter
" https://github.com/airblade/vim-gitgutter
"
" Plug 'Chiel92/vim-autoformat'
" KKPMW/vim-send-to-window
"
" Don't use default cheat sheet
let g:cheat40_use_default = 0
" Hide folds
let g:cheat40_foldlevel = 0

" }}}
" Appearance {{{

syntax enable

colorscheme gruvbox
let g:gruvbox_contrast_dark = "hard"
let g:gruvbox_contrast_light = "soft"
if $TERM == "xterm-kitty"
  set termguicolors
endif
set background=dark
let g:gruvbox_italic = 1

" }}}
" Mapping {{{

let mapleader = " "
let maplocalleader = "-"

" Easy way to edit vimrc
nnoremap <leader>ev :e $MYVIMRC<CR>
" Edit my cheat sheet
function! EditCheat40()
  let path = stdpath("config")."/cheat40.txt"
  execute 'e '.path
  " Taken from cheat40#open in vim-cheat40
  setlocal foldmethod=marker foldtext=substitute(getline(v:foldstart),'\\s\\+{{{.*$','','')
  execute 'setlocal foldlevel='.get(g:, 'cheat40_foldlevel', 1)
  setlocal concealcursor=nc conceallevel=3
  setlocal expandtab nospell nowrap textwidth=40
  setlocal fileencoding=utf-8 filetype=cheat40
  setlocal iskeyword=@,48-57,-,/,.,192-255
endfunction
nnoremap <leader>ec :call EditCheat40()<CR>
" Edit file with prefilled path from the current file
nnoremap <leader>ef :e <C-R>=expand('%:p:h') . '/'<CR>

" Find files
nnoremap <silent> <leader>ff :Files<CR>
" Find files relative to current file
nnoremap <silent> <leader>fe :call fzf#run({'sink': 'e', 'dir': expand('%:p:h') . '/'})<CR>
" Find in files
nnoremap <silent> <leader>fg :execute 'Rg ' . input('Rg/')<CR>
" Find from open buffers
nnoremap <silent> <leader>fb :Buffers<CR>

" Make escape enter normal mode in terminal as well
" FIXME only when manually launched terminal
tnoremap <Esc> <C-\><C-n>
tnoremap <C-v><Esc> <Esc>

" Special chars
inoremap <C-v>l λ
inoremap <C-v>e ◊

" Easy way to launch terminal
" FIXME hide line numbers
nnoremap <leader>tt :e term://fish<CR>
nnoremap <leader>tv :vsp term://fish<CR>
nnoremap <leader>ts :sp term://fish<CR>

" Clear screen and turn off search highlighting until the next time we search
" FIXME maybe use C-l here and move window switching functions to M-l?
nnoremap <silent> <M-l> :<C-u>nohlsearch<CR><C-l>

" Happy window switching
" Terminal mode:
tnoremap <C-h> <c-\><c-n><c-w>h
tnoremap <C-j> <c-\><c-n><c-w>j
tnoremap <C-k> <c-\><c-n><c-w>k
tnoremap <C-l> <c-\><c-n><c-w>l
" Normal mode:
nnoremap <C-h> <c-w>h
nnoremap <C-j> <c-w>j
nnoremap <C-k> <c-w>k
nnoremap <C-l> <c-w>l
" Create splits with <C-w>v and <C-w>s instead

" Open url under cursor
" FIXME better keybinding?
map <leader>u :call HandleURL()<cr>

" Trim whitespaces
" FIXME Should create a map for it to trim only the selection
nnoremap <leader>tw :silent! %s/\s\+$//<CR>:retab<CR>

" Next/prev from unimpaired.vim {{{
" [b, ]b, [B, ]B       :bprev, :bnext, :bfirst, :blast
" [l, ]l, [L, ]L       :lprev, :lnext, :lfirst, :llast
" [q, ]q, [Q, ]Q       :qprev, :qnext, :qfirst, :qlast
" Goto next/prev files by name in current folder:
" [f, ]f
"
" }}}

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
" File specific {{{
" Vimwiki {{{

" Block vimwiki from hijacking markdown files
let g:vimwiki_global_ext = 0

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
augroup END

" }}}
" Web {{{

augroup web
  autocmd!
  autocmd Filetype html setlocal ts=2 sts=2 sw=2
  autocmd Filetype javascript setlocal ts=2 sts=2 sw=2
  autocmd Filetype json setlocal ts=2 sts=2 sw=2
augroup END

" }}}
" Vim {{{

augroup vim_filetype
  autocmd!
  autocmd Filetype vim setlocal foldmethod=marker
  autocmd Filetype vim setlocal nofoldenable
  autocmd Filetype vim setlocal ts=2 sts=2 sw=2
augroup END

" }}}
" XML {{{
" FIXME can we make this work on visual selection?
command! FormatXML :%!python3 -c "import xml.dom.minidom, sys; print(xml.dom.minidom.parse(sys.stdin).toprettyxml())"
" }}}
" json {{{
" FIXME can we make this work on visual selection?
command! FormatJSON :%!python -m json.tool
" }}}
" }}}
" coc.vim {{{
" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
" }}}

"}}}
"Firenvim {{{
if exists('g:started_by_firenvim') && g:started_by_firenvim
  set laststatus=0 nonumber noruler noshowcmd
  set background=light

  augroup firenvim
    autocmd!
    autocmd BufEnter *.txt setlocal filetype=markdown
  augroup END
endif
"}}}
" vim:set sw=2 sts=2:
