" crap it's my own colorschema O,o

set background=dark
hi clear
if exists("syntax_on")
    syntax reset
endif

let g:colors_name = "aurora"

hi Normal guibg=Black guifg=White
hi StatusLine term=bold,reverse gui=bold guifg=#ffffff guibg=#656565

hi SpecialKey guifg=#f88a9a

hi Cursor guifg=bg guibg=#b70821

" hi comment guifg=lightblue
" hi label guifg=yellow

" hi NonText guifg=magenta
" hi constant guifg=cyan
" hi identifier guifg=gray
" hi statement guifg=#f6ff00
" hi preproc guifg=magenta
" hi type guifg=blue
" hi special guifg=magenta
" hi Underlined guifg=cyan
" hi operator guifg=orange gui=bold
