" Basic {{{
" Difficult to use fish as a default shell as plugins may depend on POSIX
" Instead launch terminal with fish
set shell=/bin/bash

source ~/.config/nvim/old.vim

" Enable mouse
set mouse=a
" Use CLIPBOARD register + as default
" Remember to install "xsel" for this to work!
set clipboard+=unnamedplus

" Files
set backupdir=~/.config/nvim/backup " where to put backup
set backup " make backup files
set noswapfile " just annoying when I forcefully kill vim with the recovery
set directory=~/.config/nvim/tmp,~/tmp,/tmp " store swaps here if we do enable it
set wildignore=*.swp,*.bak,*.pyc,*.class,*.o,*.obj,*.ali " ignore files for file handling
set hidden " Can change buffers without saving

" Searching
set incsearch " show search mathes as you type

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

" }}}
" Plugins {{{

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
" These are just for highlighting specific files
Plug 'https://github.com/nathangrigg/vim-beancount'
Plug 'https://github.com/dag/vim-fish.git'
Plug 'https://github.com/rust-lang/rust.vim'
Plug 'https://github.com/cespare/vim-toml.git'
Plug 'https://github.com/hail2u/vim-css3-syntax.git'
Plug 'https://github.com/wlangstroth/vim-racket'
call plug#end()

" Plugins recommended by Practical vim:
" surround.vim
"
" More fuzzy funding examples:
"https://github.com/junegunn/fzf/wiki/Examples-(vim)
"use enter key, CTRL-T, CTRL-X or CTRL-V to open selected files in the current window, in new tabs, in horizontal splits, or in vertical splits respectively.

" }}}
" Appearance {{{

syntax enable

colorscheme gruvbox
let g:gruvbox_contrast_dark = "hard"
let g:gruvbox_contrast_light = "soft"
if $TERM == "xterm-kitty"
    set background=light
    set termguicolors
else
    set background=dark
endif
let g:gruvbox_italic = 1

" }}}
" Mapping {{{

let mapleader = " "
let maplocalleader = "-"

" Fast yank/paste from PRIMARY (middle mouse buton)
nnoremap <leader>y "*y
nnoremap <leader>p "*p
nnoremap <leader>P "*P

" Easy way to edit vimrc
nnoremap <leader>ev :e $MYVIMRC<CR>
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

" Turn off search highlight after a search, will get enabled again next time
nnoremap <silent> <leader>n :silent nohlsearch<CR>
" Toggle show whitespace
nnoremap <silent> <leader>sw :set list!<CR>

" Make escape enter normal mode in terminal as well
tnoremap <Esc> <C-\><C-n>
tnoremap <C-v><Esc> <Esc>

" Special chars
inoremap <C-v>l λ
inoremap <C-v>e ◊

" Easy way to launch terminal
nnoremap <leader>tt :e term://fish<CR>
nnoremap <leader>to :vsp term://fish<CR>
nnoremap <leader>tl :sp term://fish<CR>

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
" Split and close windows
nnoremap <leader>q :q<CR>
nnoremap <leader>o :vsp<CR>
nnoremap <leader>l :sp<CR>

" Press * to search forwards for selected text, and # backwards {{{
" Type <leader>vl to toggle whitespace matching on/off
"
" http://vim.wikia.com/wiki/VimTip171
" https://vim.fandom.com/wiki/Search_for_visually_selected_text
let s:save_cpo = &cpo | set cpo&vim
if !exists('g:VeryLiteral')
  let g:VeryLiteral = 0
endif
function! s:VSetSearch(cmd)
  let old_reg = getreg('"')
  let old_regtype = getregtype('"')
  normal! gvy
  if @@ =~? '^[0-9a-z,_]*$' || @@ =~? '^[0-9a-z ,_]*$' && g:VeryLiteral
    let @/ = @@
  else
    let pat = escape(@@, a:cmd.'\')
    if g:VeryLiteral
      let pat = substitute(pat, '\n', '\\n', 'g')
    else
      let pat = substitute(pat, '^\_s\+', '\\s\\+', '')
      let pat = substitute(pat, '\_s\+$', '\\s\\*', '')
      let pat = substitute(pat, '\_s\+', '\\_s\\+', 'g')
    endif
    let @/ = '\V'.pat
  endif
  normal! gV
  call setreg('"', old_reg, old_regtype)
endfunction
vnoremap <silent> * :<C-U>call <SID>VSetSearch('/')<CR>/<C-R>/<CR>
vnoremap <silent> # :<C-U>call <SID>VSetSearch('?')<CR>?<C-R>/<CR>
vmap <kMultiply> *
nmap <silent> <Plug>VLToggle :let g:VeryLiteral = !g:VeryLiteral
  \\| echo "VeryLiteral " . (g:VeryLiteral ? "On" : "Off")<CR>
if !hasmapto("<Plug>VLToggle")
  nmap <unique> <Leader>vl <Plug>VLToggle
endif
let &cpo = s:save_cpo | unlet s:save_cpo
" }}}

" }}}
" Commands {{{
" Insert datetime
command! InsertDateTime :normal i<C-R>=system("date +%FT%T%:z")<CR>
" Insert a uuid
command! InsertUUID :normal i<C-R>=system('uuidgen')<CR>

" FIXME how to execute this?
" command! TrimWhitespace :%s/\s\+$//<CR>:retab<CR>

" Toggle between tabs or no tabs
" Defined as commands to be easier to remember
command! ExpandTab :set expandtab
command! NoExpandTab :set noexpandtab

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
augroup END

" }}}
" Vim {{{

augroup vim_filetype
    autocmd!
    autocmd Filetype vim setlocal foldmethod=marker
    autocmd Filetype vim setlocal nofoldenable
augroup END

" }}}
"
" }}}
