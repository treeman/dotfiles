"
" Vim version 7.3
" Configured with
" ./configure --with-features=huge
"             --enable-rubyinterp --disable-gui
"             --with-compiledby="Herp Derp <herp@derp.nu>"
"             --prefix=/usr

" Must be first, because it changes other options
set nocompatible " explicitly get out of vi-compatible mode

" Use pathogen to modify the runtime path to include plugin under ~/.vim/bundle
filetype off " force reloading after pathogen call
call pathogen#helptags()
call pathogen#runtime_append_all_bundles()
filetype plugin indent on " detect file type by extensions for context specifics

" Handling
set noexrc " don't use the local version of .(g)vimrc and .exrc
set modelines=0 " security exploits?
" set autochdir " always switch to the current file directory
set backspace=indent,eol,start " make backspace more flexible

set backupdir=~/.vim/backup " where to put backup
set backup " make backup files
set noswapfile " just annoying when I forcefully kill vim with the recovery
set directory=~/.vim/tmp,~/tmp,/tmp " store swaps here if we do enable it

set wildignore=*.swp,*.bak,*.pyc,*.class,*.o,*.obj,*.ali " don't care

set clipboard+=unnamed " share windows clipboard
set hidden " you can change buffers without saving

" let g:yankring_history_dir = '$HOME/.vim/tmp'

" Text formatting
set expandtab " no real tabs please!
set shiftround " when at 3 spaces, and I hit > ... go to 4, not 5
set shiftwidth=4 " auto indent amount when using indents ex >> and <<
set softtabstop=4 " when hitting tab or backspace, how wide should a tab be
set tabstop=4 " tabs width
set autoindent " keep indenting after newline
"set smartindent
set smarttab " insert tabs on the start according to shiftwidth, not tabstop

" Appearence
set background=dark
if &t_Co >= 256 || has("gui_running")
    colorscheme ir_black
endif

if &t_Co > 2 || has("syntax") || has("gui_running")
    syntax on " highlight
endif

set noerrorbells " don't make a noise
set novisualbell " don't blink

set title " change the terminals title

set relativenumber " display relative line numbers
" set number " show line numbers

set showmatch " show matching brackets
set matchtime=5 " how many tenths of a second to blink matching brackets for

set numberwidth=5 " we are good up to 99999 lines
set scrolloff=4 " keep 4 lines top and bottom for scope

set list " show tabs
set listchars=tab:>-,trail:- " show tabs and trailing

set lazyredraw " don't redraw while running macros

" UI
set laststatus=2 " always show the status line
set linespace=0 " don't insert any extra pixel lines between rows
set report=0 " tell us when anything is changed via :...
set shortmess=aOstT " shortens messages to aviod 'perss a key' prompt
set ruler " always show current positions along the bottom
set showcmd " show the command being typed
set completeopt= "don't use a pop up menu for completions

set statusline=%<%t%m%r%h%w%=%c%V,\ %l/%L\ %a\ 0x%0B\ %p%%

"set nowrap " do not wrap lines

" Searching
" use regular regexes plz
nnoremap / /\v
vnoremap / /\v

set gdefault " global searching/replacing for default

set infercase " case inferred by default
set ignorecase " case insensitive by default
set smartcase " if there are caps, go case-sensitive

set hlsearch " highlight search terms
set incsearch " show search mathes as you type

set mouse=a " use mouse everwhere

" Trying out new file settings
set encoding=utf8
set ffs=unix,dos,mac

" Platform-dependent bits
if has("win32")
    source $VIMRUNTIME/mswin.vim " windows way of copy paste
endif

" Plugin settings

let mojo_highlight_data = 1
let mojo_disable_html = 1

" Mappings
let mapleader = ","

" Toggle show whitespace, <leader> = mapleader
nmap <silent> <leader>w :set list!<CR>
" Toggle search highlighting
nmap <silent> <leader>n :silent nohlsearch<CR>

nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

nmap <silent> <leader>r :CommandTFlush<CR>

" Bind buffert toggling to f1/f2
noremap <F1> :bprev!<CR>
noremap <F2> :bnext!<CR>

" Get a list of buffers, type a number and enter for easy switching
nnoremap <F3> :buffers<CR>:buffer<Space>

map <F4> :NERDTreeToggle<CR>

" trim trailing spaces and convert tabs to spaces
map <F5> :silent! %s/\s\+$//<CR>:retab<CR>
" nnoremap <leader>w :silent! %s/\s\+$//<CR>:retab<CR>

" Toggle between tabs or no tabs
map <F6> :set expandtab<CR>
map <F7> :set noexpandtab<CR>

" Shift-tab to insert hard tab
imap <silent> <S-tab> <C-V><tab>

" Reselect just pasted text
nnoremap <leader>v V`]

" Remove arrow keys :)
map <down> <nop>
map <left> <nop>
map <right> <nop>
map <up> <nop>

imap <down> <nop>
imap <left> <nop>
imap <right> <nop>
imap <up> <nop>

" Move screen lines instead of real lines
nnoremap j gj
nnoremap k gk

" fix mswin overriding my visual block mode
map <C-V> <C-V>

" Easy window switching and closing
map <C-J> <C-W>j
map <C-K> <C-W>k
map <C-H> <C-W>h
map <C-L> <C-W>l
nmap <leader>q :q<CR>
nmap <leader>o <C-w>v<C-w>l
nmap <leader>l <C-w>s

" Allow deleteing without updating the clipboard (yank buffer)
vnoremap x "_x
vnoremap X "_X

nmap <silent> <leader>d "_d
vmap <silent> <leader>d "_d

" Quick yank to the end of the line
nmap Y y$

" Fast yank/paste to OS clipboard
nmap <leader>y "+y
nmap <leader>Y "+yy
nmap <leader>p "+p
nmap <leader>P "+P

" Don't move cursor after paste
noremap p p`[
noremap P P`[

" Quick alignment of text
nmap <leader>al :left<CR>
nmap <leader>ar :right<CR>
nmap <leader>ac :center<CR>

" Sudo to write
cmap w!! w !sudo tee % >/dev/null

" Jump to matching pairs easily, with Tab
nnoremap <Tab> %
vnoremap <Tab> %

