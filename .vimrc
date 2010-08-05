" Basics {
    if has("syntax")
        syntax on " highlight
        set t_Co=256
        colorscheme ir_black
    endif

    set nocompatible " explicitly get out of vi-compatible mode
    set noexrc " don't use the local version of .(g)vimrc and .exrc
    set background=dark
" }

" General {
    set autochdir " always switch to the current file directory
    set backspace=indent,eol,start " make backspace more flexible

    set backupdir=~/.vim/backup " where to put backup
    set backup " make backup files
    set directory=~/.vim/tmp " directory to place swap files in

    set clipboard+=unnamed " share windows clipboard
    set hidden " you can change buffers without saving
    set mouse=a " use mouse everwhere
    set noerrorbells " don't make a noise

    " The only sensible setup for Unicode editing.
    if v:version >= 600
        set encoding=utf-8
        set termencoding=utf-8
        set fileencodings=ucs-bom,ascii,utf-8,latin1
    endif

    " Platform-dependent bits
    if has("win32")
        set fileformats=dos,unix,mac
        source $VIMRUNTIME/mswin.vim " windows way of copy paste
    elseif has("unix")
        set fileformats=unix,dos,mac
    endif

    " :language en
" }

" Vim UI {
    " set cursorcolumn " highlight the current column
    set incsearch " highlight as you type search phrase
    set laststatus=2 " always show the status line
    set lazyredraw " don't redraw while running macros
    set linespace=0 " don't insert any extra pixel lines between rows
    set matchtime=5 " how many tenths of a second to blink matching brackets for
    set nostartofline " leave my cursor where it was
    set novisualbell " don't blink
    set number " turn on line numbers
    set numberwidth=5 " we are good up to 99999 lines
    set report=0 " tell us when anything is changed via :...
    set ruler " always show current positions along the bottom
    set scrolloff=10 " keep 10 lines top and bottom for scope
    set shortmess=aOstT " shortens messages to aviod 'perss a key' prompt
    set showcmd " show the command being typed
    set showmatch " show matching brackets
    set sidescrolloff=10 " Keep 5 lines at the size

    set list " show tabs
    set listchars=tab:>-,trail:- "show tabs and trailing
    set clipboard=unnamed "yank to cross-vim register, after you close it

    " set statusline=%F%m%r%h%w[%L][%{&ff}]%y[%p%%][%04l,%04v]
        " %F: full path
        " %m%r%h%w: modified/readonly/help/preview flags
        " [%L]: num lines
        " [%{&ff}]: fileformat
        " %y: curr syntax
        " [%p%%]: current % into file
        " %041: curr line
        " %04v: curr column
        " %041: curr line
        " %04v: curr column
    " cream:
    " filename |fileformat:encode:??|?? crap wrap column width:left tabs:4 col/row
    " set statusline=%<%F\ %h%m%r%=%-14.(%l,%c%V%)\ %P
    set statusline=%<%t%m%r%h%w%=%c%V,\ %l/%L\ %a\ 0x%0B\ %p%%
" }

" Text formatting/layout {
    set completeopt= "don't use a pop up menu for completions
    set expandtab " no real tabs please!
    set ignorecase " case insensitive by default
    set infercase " case inferred by default
    set nowrap " do not wrap lines
    set shiftround " when at 3 spaces, and I hit > ... go to 4, not 5
    set smartcase " if there are caps, go case-sensitive
    set shiftwidth=4 " auto indent amount when using indents ex >> and <<
    set softtabstop=4 " when hitting tab or backspace, how many spaces should a tab be
    set tabstop=4 " tabs width
    set autoindent " keep indenting after newline
    set smartindent
" }

" folding

    set foldenable " turn on folding
    set foldmarker={,} " fold c style brackets
    set foldmethod=marker " fold on the marker
    set foldlevel=100 " don't autofold anything, but I can still fold manually
    set foldopen=block,hor,mark,percent,quickfix,tag " what movements opens the fold

    " custom fold text function
    " cleaner than the default
    function! SimpleFoldText()
        return getline(v:foldstart).' '
    endfunction
    set foldtext=SimpleFoldText()

" Mappings {
    " Bind buffert toggling to f1/f2
    noremap <f1> :bprev!<CR>
    noremap <f2> :bnext!<CR>

    map <F4> :NERDTreeToggle<CR>

    " trim trailing spaces and convert tabs to spaces
    map <F5> :silent! %s/\s\+$//<CR>:retab<CR>

    map <F6> :set expandtab<CR>
    map <F7> :set noexpandtab<CR>

    " Remove arrow keys :)
    map <down> <nop>
    map <left> <nop>
    map <right> <nop>
    map <up> <nop>

    imap <down> <nop>
    imap <left> <nop>
    imap <right> <nop>
    imap <up> <nop>

    imap <F6> \t

    " fix mswin overriding my visual block mode
    map <C-V> <C-V>
" }
