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

" Make escape enter normal mode in terminal as well
tnoremap <Esc> <C-\><C-n>
tnoremap <C-v><Esc> <Esc>

" Launch a terminal in a split window with fish
map <leader>t :vsp term://fish<CR>
" :e term://fish if we don't want a split window, maybe override :te?

" Happy window switching
" Terminal mode:
tnoremap <C-h> <c-\><c-n><c-w>h
tnoremap <C-j> <c-\><c-n><c-w>j
tnoremap <C-k> <c-\><c-n><c-w>k
tnoremap <C-l> <c-\><c-n><c-w>l
" Insert mode:
inoremap <C-h> <Esc><c-w>h
inoremap <C-j> <Esc><c-w>j
inoremap <C-k> <Esc><c-w>k
inoremap <C-l> <Esc><c-w>l
" Visual mode:
vnoremap <C-h> <Esc><c-w>h
vnoremap <C-j> <Esc><c-w>j
vnoremap <C-k> <Esc><c-w>k
vnoremap <C-l> <Esc><c-w>l
" Normal mode:
nnoremap <C-h> <c-w>h
nnoremap <C-j> <c-w>j
nnoremap <C-k> <c-w>k
nnoremap <C-l> <c-w>l
" Split and close windows
nmap <leader>q :q<CR>
nmap <leader>o :vsp<CR>
nmap <leader>l :sp<CR>
