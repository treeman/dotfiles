
" Appearence
if has("syntax")
    syntax on " highlight
    set t_Co=256
    colorscheme ir_black
endif
set background=dark

set noerrorbells " don't make a noise
set novisualbell " don't blink

set relativenumber " display relative line numbers

set incsearch " highlight as you type search phrase
set lazyredraw " don't redraw while running macros

set showmatch " show matching brackets
set matchtime=5 " how many tenths of a second to blink matching brackets for

set numberwidth=5 " we are good up to 99999 lines
set scrolloff=10 " keep 10 lines top and bottom for scope

set list " show tabs
set listchars=tab:>-,trail:- " show tabs and trailing

" UI
set laststatus=2 " always show the status line
set linespace=0 " don't insert any extra pixel lines between rows
set report=0 " tell us when anything is changed via :...
set shortmess=aOstT " shortens messages to aviod 'perss a key' prompt
set ruler " always show current positions along the bottom
set showcmd " show the command being typed
set completeopt= "don't use a pop up menu for completions

" set statusline=%F%m%r%h%w[%L][%{&ff}]%y[%p%%][%04l,%04v]
    " %F: full path
    " %m%r%h%w: modified/readonly/help/preview flags
    " [%L]: num lines
    " [%{&ff}]: fileformat
    " %y: curr syntax
    " [%p%%]: current % into file
    " %041: curr line
    " %04v: curr column
" cream:
" filename |fileformat:encode:??|?? crap wrap column width:left tabs:4 col/row
" set statusline=%<%F\ %h%m%r%=%-14.(%l,%c%V%)\ %P
set statusline=%<%t%m%r%h%w%=%c%V,\ %l/%L\ %a\ 0x%0B\ %p%%

" Text formatting
set expandtab " no real tabs please!
set shiftround " when at 3 spaces, and I hit > ... go to 4, not 5
set shiftwidth=4 " auto indent amount when using indents ex >> and <<
set softtabstop=4 " when hitting tab or backspace, how many spaces should a tab be
set tabstop=4 " tabs width
set autoindent " keep indenting after newline
set smartindent

"set nowrap " do not wrap lines

" Searching
" Use regular regexes plz
nnoremap / /\v
vnoremap / /\v

set infercase " case inferred by default
set ignorecase " case insensitive by default
set smartcase " if there are caps, go case-sensitive

set incsearch
set hlsearch

" Handling
set nocompatible " explicitly get out of vi-compatible mode
set noexrc " don't use the local version of .(g)vimrc and .exrc

set modelines=0 " security exploits?

set autochdir " always switch to the current file directory
set backspace=indent,eol,start " make backspace more flexible

set backupdir=~/.vim/backup " where to put backup
set backup " make backup files
set directory=~/.vim/tmp " directory to place swap files in

set clipboard+=unnamed " share windows clipboard
set hidden " you can change buffers without saving

set mouse=a " use mouse everwhere

" The only sensible setup for Unicode editing.
if has("multi_byte")
    set bomb
    set encoding=utf-8
    setglobal fileencoding=utf-8 bomb
    set fileencodings=ucs-bom,utf-8,latin1
    set fenc=utf-8 " save files with åäö kthx

    if &termencoding == ""
        let &termencoding = &encoding
    endif

    "If it doesn't work maybe just use
    "set fileencoding=utf-8
endif

" Platform-dependent bits
if has("win32")
    set fileformats=dos,unix,mac
    source $VIMRUNTIME/mswin.vim " windows way of copy paste
elseif has("unix")
    set fileformats=unix,dos,mac
endif

" Mappings

let mapleader = ","

" Toggle show whitespace, <leader> = mapleader
nmap <silent> <leader>l :set list!<CR>
" Toggle search highlighting
nmap <silent> <leader>n :silent nohlsearch<CR>

" Bind buffert toggling to f1/f2
noremap <f1> :bprev!<CR>
noremap <f2> :bnext!<CR>

" Get a list of buffers, type a number and enter for easy switching
nnoremap <F3> :buffers<CR>:buffer<Space>

map <F4> :NERDTreeToggle<CR>

" trim trailing spaces and convert tabs to spaces
map <F5> :silent! %s/\s\+$//<CR>:retab<CR>
nnoremap <leader>w :silent! %s/\s\+$//<CR>:retab<CR>

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

" Allow deleteing without updating the clipboard (yank buffer)
vnoremap x "_x
vnoremap X "_X

" Don't move cursor after paste
noremap p p`[
noremap P P`[

