" Note that neovim config files now exist in ~/.config (base $XDG_CONFIG_HOME)
" ~/.config/nvim ~/.config/nvim/init.vim

" Workaround as vim sometimes depends on POSIX functionality
set shell=/bin/bash

" Plugin handling using vim-plug
" Do :PlugInstall to install the plugins
call plug#begin('~/.config/nvim/plugged')
Plug 'https://github.com/dag/vim-fish.git'
Plug 'https://github.com/morhetz/gruvbox.git'
Plug 'https://github.com/chriskempson/base16-vim.git'
Plug 'https://github.com/rust-lang/rust.vim'
Plug 'https://github.com/cespare/vim-toml.git'
Plug 'https://github.com/scrooloose/nerdcommenter.git'
Plug 'https://github.com/benekastah/neomake.git'
Plug 'https://github.com/Floobits/floobits-neovim.git'
Plug 'https://github.com/vim-perl/vim-perl.git'
Plug 'https://github.com/elixir-lang/vim-elixir.git'
Plug 'https://github.com/avdgaag/vim-phoenix'
Plug 'https://github.com/slashmili/alchemist.vim'
Plug 'https://github.com/vimwiki/vimwiki.git', { 'branch': 'dev' }
Plug 'https://github.com/pangloss/vim-javascript.git'
Plug 'https://github.com/hail2u/vim-css3-syntax.git'
" Plug 'https://github.com/blindFS/vim-taskwarrior' " Some problem with sync
Plug 'https://github.com/tbabej/taskwiki.git'
Plug 'https://github.com/EinfachToll/DidYouMean.git'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' } " May want to customize it more, very powerful
Plug 'junegunn/fzf.vim'
Plug 'https://github.com/tpope/vim-fugitive.git' " Git plugin. Need to integrate it to workflow!
call plug#end()

filetype plugin indent on

" File specific
autocmd Filetype html setlocal ts=2 sts=2 sw=2
autocmd Filetype javascript setlocal ts=2 sts=2 sw=2

" vimwiki
let g:taskwiki_disable_concealcursor = 1

" Appearance
syntax enable
set background=dark
let g:gruvbox_contrast_dark = "hard"
colorscheme gruvbox

set relativenumber " display relative line numbers
set number " show line numbers

set list " show tabs
set listchars=tab:>-,trail:- " show tabs and trailing

" Text formatting
set expandtab " no real tabs please!
set shiftround " when at 3 spaces, and I hit > ... go to 4, not 5
set shiftwidth=4 " auto indent amount when using indents ex >> and <<
set softtabstop=4 " when hitting tab or backspace, how wide should a tab be
set tabstop=4 " tabs width
set autoindent " keep indenting after newline
set smarttab " insert tabs on the start according to shiftwidth, not tabstop

" UI
set laststatus=2 " always show the status line
set linespace=0 " don't insert any extra pixel lines between rows
set report=0 " tell us when anything is changed via :...
set shortmess=aOstT " shortens messages to aviod 'perss a key' prompt
set ruler " always show current positions along the bottom
set showcmd " show the command being typed
set completeopt= "don't use a pop up menu for completions

set statusline=%<%t%m%r%h%w%=%c%V,\ %l/%L\ %a\ 0x%0B\ %p%%

" Files etc
set backupdir=~/.config/nvim/backup " where to put backup
set backup " make backup files
set noswapfile " just annoying when I forcefully kill vim with the recovery
set directory=~/.config/nvim/tmp,~/tmp,/tmp " store swaps here if we do enable it
set wildignore=*.swp,*.bak,*.pyc,*.class,*.o,*.obj,*.ali " ignore files for file handling
set hidden " Can change buffers without saving

" Searching
set nohlsearch " don't highlight search terms
set incsearch " show search mathes as you type

" Mappings
let mapleader = " "

" Toggle show whitespace, <leader> = mapleader
nmap <silent> <leader>w :set list!<CR>
" Toggle search highlighting
nmap <silent> <leader>n :silent nohlsearch<CR>

" Bind buffert toggling to f1/f2
noremap <F1> :bprev!<CR>
noremap <F2> :bnext!<CR>

" Jump to previously opened buffer
noremap <F3> :b#<CR>

" trim trailing spaces and convert tabs to spaces
map <F5> :silent! %s/\s\+$//<CR>:retab<CR>

" Toggle between tabs or no tabs
map <F6> :set expandtab<CR>
map <F7> :set noexpandtab<CR>

" Spell checking
map <F10> :set spell! spell?<CR>

" Edit file with prefilled path from the current file
nnoremap <leader>e :e <C-R>=expand('%:p:h') . '/'<CR>
" Find with zfz
nnoremap <silent> <leader><space> :Files<CR>
nnoremap <silent> <leader>b :Buffers<CR>
nnoremap <silent> <leader>/ :execute 'Ag ' . input('Ag/')<CR>
nnoremap <silent> <leader>f :call fzf#run({'sink': 'e', 'dir': expand('%:p:h') . '/'})<CR>
" Find git commits
nnoremap <silent> <leader>gc :Commits<CR>
nnoremap <silent> <leader>gb :BCommits<CR>

" Shift-tab to insert hard tab
imap <silent> <S-tab> <C-V><tab>

" Remove arrow keys :)
map <down> <nop>
map <left> <nop>
map <right> <nop>
map <up> <nop>

" Easy window handling
map <C-J> <C-W>j
map <C-K> <C-W>k
map <C-H> <C-W>h
map <C-L> <C-W>l
nmap <leader>q :q<CR>
nmap <leader>o <C-w>v<C-w>l
nmap <leader>l <C-w>s

" Jump to matching pairs easily, with Tab
nnoremap <Tab> %
vnoremap <Tab> %

" Move through visual lines
nnoremap <silent> k gk
nnoremap <silent> j gj

" Use CLIPBOARD register + as default
set clipboard+=unnamedplus

set mouse=a

" Fast yank/paste
" PRIMARY (middle mouse button) is found with *
" CLIPBOARD  is found with +
nmap <leader>y "*y
nmap <leader>Y "*yy
nmap <leader>p "*p
nmap <leader>P "*P

" Yank mouse selection with ctrl c
vmap <C-C> "*y

" Pretty format json:
" :%!python -m json.tool

" Quickfix
" :copen    " Open quickfix window
" :cw       " Open if there are errors, otherwise close it
" :cc       " Show current error
" :cn       " Goto next error
" :cnf      " Go to first error in next file
" :cN       " Goto prev error
" :set makeprg=cargo\ build
" :Neomake doesn't really work...

" Ctrl alt p
map  :make<CR>:cw<CR>
" Ctrl alt x
map <C-M-X> :make clean<CR>
"map  :Neomake!<CR>:cw<CR>
" Next, previous errors and goto current error
map <C-N> :cn<CR>
map <C-P> :cN<CR>
map <C-C> :cc<CR>

" Just for now, could make things better later.
" Bind automatic run like this if you're doing things?
" map <C-X> :! cd bin && ./ld33<CR><CR>

" Tags
" C-] jump to tag
" C-Alt-] jump to tag, select 
map  g<C-]>
" C-= jump back (default C-T)
map <C-=> :pop<CR>
" C-' previous matching tag
map ' :tp<CR>
" C-\ next matching tag
map  :tn<CR>
" try
" set tags=tags/;
" set tags=./tags
" rerun tags -R sometimes with Ctrl alt \
map  :! ctags -R<CR>

set tags=./tags;/

" C-x C-o omnicomplete
" C-n C-p to toggle between. Nice!

