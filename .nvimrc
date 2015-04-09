filetype plugin indent on

" Appearance
syntax on
colorscheme ir_black

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

" Searching
set hlsearch " highlight search terms
set incsearch " show search mathes as you type

" Mappings
let mapleader = ","

" Toggle show whitespace, <leader> = mapleader
nmap <silent> <leader>w :set list!<CR>
" Toggle search highlighting
nmap <silent> <leader>n :silent nohlsearch<CR>

" Bind buffert toggling to f1/f2
noremap <F1> :bprev!<CR>
noremap <F2> :bnext!<CR>

" trim trailing spaces and convert tabs to spaces
map <F5> :silent! %s/\s\+$//<CR>:retab<CR>

" Toggle between tabs or no tabs
map <F6> :set expandtab<CR>
map <F7> :set noexpandtab<CR>

" Spell checking
map <F10> :set spell! spell?<CR>

" Shift-tab to insert hard tab
imap <silent> <S-tab> <C-V><tab>

" Remove arrow keys :)
map <down> <nop>
map <left> <nop>
map <right> <nop>
map <up> <nop>

imap <down> <nop>
imap <left> <nop>
imap <right> <nop>
imap <up> <nop>

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
