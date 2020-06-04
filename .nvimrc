" Difficult to use fish as a default shell as plugins may depend on POSIX
" Instead launch terminal with fish
set shell=/bin/bash

let mapleader = " "
let maplocalleader = "-"

source ~/.config/nvim/old.vim

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

" Plugins recommended by Practical vim:
" surround.vim
"
" More fuzzy funding examples:
"https://github.com/junegunn/fzf/wiki/Examples-(vim)
"use enter key, CTRL-T, CTRL-X or CTRL-V to open selected files in the current window, in new tabs, in horizontal splits, or in vertical splits respectively.

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

" Make escape enter normal mode in terminal as well
tnoremap <Esc> <C-\><C-n>
tnoremap <C-v><Esc> <Esc>

" Easy way to launch terminal
nnoremap <leader>tt :e term://fish<CR>
nnoremap <leader>to :vsp term://fish<CR>
nnoremap <leader>tl :sp term://fish<CR>

" Special chars
inoremap <C-L> λ
inoremap <C-E> ◊

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

" Insert datetime
command! InsertDateTime :normal i<C-R>=system("date +%FT%T%:z")<CR>
" Insert a uuid
command! InsertUUID :normal i<C-R>=system('uuidgen')<CR>

" Block vimwiki from hijacking markdown files
let g:vimwiki_global_ext = 0

" Counts the number of # in markdown headers
function! MarkdownLevel()
    let h = matchstr(getline(v:lnum), '^#\+')
    if empty(h)
        return "="
    else
        return ">" . len(h)
    endif
endfunction

augroup markdowngroup
    autocmd!
    autocmd FileType markdown setlocal foldexpr=MarkdownLevel()
    autocmd FileType markdown setlocal foldmethod=expr
    autocmd FileType markdown :normal zR
augroup END

augroup beancountgroup
    autocmd!
    autocmd FileType beancount setlocal foldexpr=MarkdownLevel()
    autocmd FileType beancount setlocal foldmethod=expr
augroup END

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

augroup web
    autocmd!
    autocmd Filetype html setlocal ts=2 sts=2 sw=2
    autocmd Filetype javascript setlocal ts=2 sts=2 sw=2
augroup END

