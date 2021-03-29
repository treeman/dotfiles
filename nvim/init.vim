" Installation {{{
" Update plugins:
"   :PlugUpdate
"
" Install all available treesitter parsers:
"   :TSInstall all
"
" Dependencies:
"   git, ripgrep, fd, bat, go
"   pip3 install --upgrade pynvim
"
" LSP servers:
"   rust-analyzer:
"     https://github.com/rust-analyzer/rust-analyzer
"     cargo xtask install --server
"
"   elixir-ls:
"     https://github.com/elixir-lsp/elixir-ls.git
"     mix compile
"     mix elixir_ls.release -o release
"     Ensure that $ELIXIR_LS_LANGUAGE_SERVER points to release/language_server.sh
"
"   tsserver:
"     npm install -g typescript typescript-language-server
" }}}
" Basic {{{
" Difficult to use fish as a default shell as plugins may depend on POSIX
" Instead launch terminal with fish
set shell=/bin/bash

" Enable mouse
set mouse=a
" Use CLIPBOARD register + as default
" Remember to install "xsel" for this to work!
" For Neovim in WSL see:
" https://github.com/neovim/neovim/wiki/FAQ#how-to-use-the-windows-clipboard-from-wsl
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
set lazyredraw " speed up macros

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
set statusline=%<%{FugitiveStatusline()}%=%t\ %m%r%h%w%y%=%c%V,\ %l/%L\ %a\ 0x%0B\ %p%%
set laststatus=2 " always show the status line
set linespace=0 " don't insert any extra pixel lines between rows
set report=0 " tell us when anything is changed via :...
set shortmess=aOstTc " shortens messages to aviod 'perss a key' prompt
set ruler " always show current positions along the bottom
set showcmd " show the command being typed
set signcolumn=yes " Use a gutter for git-gutter and LSP messages
set completeopt=menuone,noselect " Required settings for nvim-compe

augroup CursorLineOnlyInActiveWindow
  autocmd!
  autocmd VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  autocmd WinLeave * setlocal nocursorline
augroup END

" Use gx to open urls in firefox
let g:netrw_browsex_viewer = "firefox"

" Must set leaders before plugins
let mapleader = " "
let maplocalleader = "-"

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
" Another fuzzy finder
Plug 'nvim-telescope/telescope.nvim'
" Personal wiki
Plug 'https://github.com/vimwiki/vimwiki.git', { 'branch': 'dev' }
" Avoid mistyping filenames, ask which file to open if file not find
Plug 'https://github.com/EinfachToll/DidYouMean.git'
" Easy way to comment things
Plug 'https://github.com/tomtom/tcomment_vim'
" For many more features see tcomment_vim
" This is also an option
" Plug 'https://github.com/b3nj5m1n/kommentary', { 'branch': 'main' }
" Enhance the '.' operator
Plug 'https://github.com/tpope/vim-repeat'
" Easily surround things
Plug 'https://github.com/tpope/vim-surround'
" More targets
Plug 'https://github.com/wellle/targets.vim'
" Highlight what was yanked
Plug 'https://github.com/machakann/vim-highlightedyank'
" Vim cheat sheet
Plug 'https://github.com/lifepillar/vim-cheat40'
" Generic lua plugins
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
" Indentation lines for visual aid
Plug 'https://github.com/lukas-reineke/indent-blankline.nvim', { 'branch': 'lua' }
" Make * and # search visually
Plug 'https://github.com/nelstrom/vim-visual-star-search'
" Change f, F, t and T to be smarter
Plug 'https://github.com/rhysd/clever-f.vim'
" Use s as a two-char f
Plug 'https://github.com/justinmk/vim-sneak'

" File explorer
Plug 'https://github.com/lambdalisue/fern.vim'
Plug 'https://github.com/lambdalisue/nerdfont.vim'
Plug 'https://github.com/lambdalisue/fern-renderer-nerdfont.vim'
Plug 'https://github.com/lambdalisue/fern-git-status.vim'
Plug 'https://github.com/lambdalisue/glyph-palette.vim'
" Fixes some performance issues
Plug 'antoinemadec/FixCursorHold.nvim'

" Specific file support
Plug 'https://github.com/nathangrigg/vim-beancount'
Plug 'https://github.com/dag/vim-fish.git'
Plug 'https://github.com/rust-lang/rust.vim'
Plug 'https://github.com/cespare/vim-toml.git'
Plug 'https://github.com/hail2u/vim-css3-syntax.git'
Plug 'https://github.com/wlangstroth/vim-racket'
Plug 'https://github.com/otherjoel/vim-pollen.git'
Plug 'https://github.com/elixir-editors/vim-elixir.git'
Plug 'mhinz/vim-mix-format'

" Automatically insert end in insert mode for some languages
Plug 'https://github.com/tpope/vim-endwise'
" Light statusbar
Plug 'https://github.com/itchyny/lightline.vim'
Plug 'shinchu/lightline-gruvbox.vim'
" Treesitter syntax highlighting
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'romgrk/nvim-treesitter-context'
" Dispatch async things
Plug 'https://github.com/tpope/vim-dispatch'
" Maximize for windows
Plug 'https://github.com/szw/vim-maximizer'
" Autocompletion framework with a ton of support
Plug 'https://github.com/hrsh7th/nvim-compe'

" LSP support
" See doc :help lsp
" Collection of common configurations for the Nvim LSP client
Plug 'neovim/nvim-lspconfig'
" Extensions to built-in LSP, for example, providing type inlay hints
Plug 'tjdevries/lsp_extensions.nvim'

" Autoformat for different languages
Plug 'Chiel92/vim-autoformat'

" Git plugins
Plug 'lewis6991/gitsigns.nvim'
Plug 'https://github.com/tpope/vim-fugitive'
Plug 'https://github.com/rbong/vim-flog'
Plug 'https://github.com/rhysd/git-messenger.vim'

" nvim in Firefox
Plug 'https://github.com/glacambre/firenvim', { 'do': { _ -> firenvim#install(0) } }
" Peek registry contents, for easy use of " and @
Plug 'https://github.com/junegunn/vim-peekaboo'
" Display colors
Plug 'rrethy/vim-hexokinase', { 'do': 'make hexokinase' }
" Sudo write for neovim
Plug 'https://github.com/lambdalisue/suda.vim'
" Kill buffers smartly
Plug 'mhinz/vim-sayonara', { 'on': 'Sayonara' }
call plug#end()

" }}}
" Appearance {{{

syntax enable

let g:gruvbox_contrast_dark = "hard"
let g:gruvbox_contrast_light = "soft"
if $TERM == "xterm-kitty" || $TERM == "alacritty" || $TERM == "xterm-256color"
  set termguicolors
endif
set background=dark
let g:gruvbox_italic = 1
let g:gruvbox_bold = 1
let g:gruvbox_italicize_comments = 1

" To avoid the low contrast gray on 2nd left side
let g:lightline_gruvbox_style = 'hard_left'
colorscheme gruvbox

" For more info see:
" :h statusline
" :h g:lightline.component
let g:lightline = {
  \ 'colorscheme': 'gruvbox',
  \ 'active': {
  \   'left': [ [ 'mode', 'paste' ],
  \             [ 'gitbranch', 'gitstatus', 'readonly', 'modified' ],
  \             [ 'filename' ] ],
  \   'right': [ [ 'lineinfo' ],
  \              [ 'charvaluehex' ],
  \              [ 'lsp', 'spell', 'fileformat', 'fileencoding', 'filetype' ] ]
  \ },
  \ 'inactive': {
  \   'left': [ [ 'filename' ] ],
  \   'right': [ [ 'lineinfo' ] ]
  \ },
  \ 'component': {
  \   'charvaluehex': '0x%0B',
  \   'lineinfo': '%c:%l/%L%<'
  \ },
  \ 'component_function': {
  \   'gitbranch': 'FugitiveHead',
  \   'gitstatus': 'LightlineGitStatus',
  \   'spell': 'LightlineSpell',
  \   'filename': 'LightlineFilename',
  \   'fileformat': 'LightlineFileformat',
  \   'filetype': 'LightlineFiletype',
  \   'fileencoding': 'LightlineFileEncoding',
  \   'lsp': 'LightlineLSP',
  \ },
  \ 'mode_map': {
  \   'n' : 'N',
  \   'i' : 'I',
  \   'R' : 'R',
  \   'v' : 'V',
  \   'V' : 'VL',
  \   "\<C-v>": 'VB',
  \   'c' : 'C',
  \   's' : 'S',
  \   'S' : 'SL',
  \   "\<C-s>": 'SB',
  \   't': 'T',
  \ },
\ }

" These functions truncates away a bunch of stuff when width is smaller
function! LightlineGitStatus()
  if winwidth(0) <= 95
    return ""
  endif

  return get(b:,'gitsigns_status','')
endfunction

function! LightlineFilename()
  return winwidth(0) > 110 ? expand('%:f') : expand('%:t')
endfunction

function! LightlineSpell()
  return winwidth(0) > 70 ? (&spell ? &spelllang : '') : ''
endfunction

function! LightlineFileformat()
  return winwidth(0) > 95 ? &fileformat : ''
endfunction

function! LightlineFiletype()
  return winwidth(0) > 95 ? (&filetype !=# '' ? &filetype : 'no ft') : ''
endfunction

function! LightlineFileEncoding()
  return winwidth(0) > 95 ? (&fenc !=# '' ? &fenc : &enc) : ''
endfunction

function! LightlineLSP()
  let no_lsp = luaeval('next(vim.lsp.buf_get_clients()) == nil')
  if no_lsp
    return ''
  else
    return 'LSP'
  endif
endfunction

" Workaround for https://github.com/lukas-reineke/indent-blankline.nvim/issues/59
set colorcolumn=99999

" This is too verbose unfortunately
" function! LightlineTreesitter()
"   return winwidth(0) > 70 ? nvim_treesitter#statusline(40) : ''
" endfunction
" }}}
" Mapping {{{

" Reload vimrc
nnoremap <leader>sv :so $MYVIMRC<CR>
" Easy way to edit vimrc
nnoremap <leader>ev :e $MYVIMRC<CR>
" Edit my cheat sheet
function! EditCheat40()
  let path = stdpath("config")."/cheat40.txt"
  execute 'e '.path
  " Taken from cheat40#open in vim-cheat40
  setlocal foldmethod=marker foldtext=substitute(getline(v:foldstart),'\\s\\+{.*$','','')
  execute 'setlocal foldlevel='.get(g:, 'cheat40_foldlevel', 1)
  setlocal concealcursor=nc conceallevel=3
  setlocal expandtab nospell nowrap textwidth=40
  setlocal fileencoding=utf-8 filetype=cheat40
  setlocal iskeyword=@,48-57,-,/,.,192-255
endfunction
nnoremap <leader>ec :call EditCheat40()<CR>
" Edit file with prefilled path from the current file
nnoremap <leader>ef :e <C-R>=expand('%:p:h') . '/'<CR>
" Allow saving of files as sudo when I forgot to start vim using sudo.
"cmap w!! w !sudo tee > /dev/null %
"com -bar W exe 'w !sudo tee >/dev/null %:p:S' | setl nomod
cmap w!! SudaWrite

" Load notes
nnoremap <leader>n :e ~/vimwiki/projects/

" Find files
"nnoremap <silent> <leader>ff :Files<CR>
nnoremap <silent> <leader>ff :Telescope find_files<CR>
" Find files relative to current file
nnoremap <silent> <leader>fe :call fzf#run({'sink': 'e', 'dir': expand('%:p:h') . '/'})<CR>
" Find in files
nnoremap <silent> <leader>/ :execute 'Rg ' . input('Rg/')<CR>
" Find from open buffers
nnoremap <silent> <leader>fb :Buffers<CR>

" CTRL-A CTRL-Q to select all and build quickfix list
function! s:build_quickfix_list(lines)
  call setqflist(map(copy(a:lines), '{ "filename": v:val }'))
  copen
  cc
endfunction

let g:fzf_action = {
  \ 'ctrl-q': function('s:build_quickfix_list'),
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }

let $FZF_DEFAULT_OPTS = '--bind ctrl-a:select-all'

" Copy/paste to mouse clipboard quickly
nnoremap <silent> <leader>p "*p
nnoremap <silent> <leader>P "*P
nnoremap <silent> <leader>y "*y
nnoremap <silent> <leader>Y "*Y
" Don't overwrite register when pasting in visual selection
xnoremap p "_dP

" Special chars
"inoremap <C-l> λ
"inoremap <C-e> ◊
"inoremap <C-v>l λ
"inoremap <C-v>e ◊

" Easy way to launch terminal
function! FishTerm()
  execute 'e term://fish'
  " Make escape enter normal mode in terminal as well
  tnoremap <buffer> <Esc> <C-\><C-n>
  tnoremap <buffer> <C-v><Esc> <Esc>
  setlocal nonumber
  setlocal norelativenumber
endfunction
nnoremap <leader>tt :call FishTerm()<CR>
nnoremap <leader>tv :vs <bar> :call FishTerm()<CR>
nnoremap <leader>ts :sp <bar> :call FishTerm()<CR>

" Clear screen and turn off search highlighting until the next time we search
nnoremap <silent> <M-l> :<C-u>nohlsearch<CR><C-l>

" Happy window switching
" Terminal mode:
tnoremap <C-h> <c-\><c-n><c-w>h
tnoremap <C-j> <c-\><c-n><c-w>j
tnoremap <C-k> <c-\><c-n><c-w>k
tnoremap <C-l> <c-\><c-n><c-w>l
tnoremap <C-left> <c-w>h
tnoremap <C-down> <c-w>j
tnoremap <C-up> <c-w>k
tnoremap <C-right> <c-w>l
" Normal mode:
nnoremap <C-h> <c-w>h
nnoremap <C-j> <c-w>j
nnoremap <C-k> <c-w>k
nnoremap <C-l> <c-w>l
nnoremap <C-left> <c-w>h
nnoremap <C-down> <c-w>j
nnoremap <C-up> <c-w>k
nnoremap <C-right> <c-w>l
" Create splits with <C-w>v and <C-w>s, or :sp and :vs
" Close the current buffer if it's not shown in another window, but keep the window itself
nnoremap <leader>s :Sayonara!<CR>

let g:maximizer_set_default_mapping = 0
nnoremap <silent><C-w>m :MaximizerToggle<CR>

" Goto previous buffer
nnoremap <leader>b :edit #<CR>

" Toggle chadtree
"nnoremap <leader>v <cmd>CHADopen<cr>
"nnoremap <leader>t :NvimTreeToggle<CR>
nnoremap <leader>v :Fern . -drawer<CR>

" Completion
inoremap <silent><expr> <C-Space> compe#complete()
inoremap <silent><expr> <C-e> compe#close('<C-e>')
"inoremap <silent><expr> <CR>      compe#confirm('<CR>')
"inoremap <silent><expr> <C-f>     compe#scroll({ 'delta': +4 })
"inoremap <silent><expr> <C-d>     compe#scroll({ 'delta': -4 })

" Trim whitespaces
" FIXME Should create a map for it to trim only the selection
nnoremap <leader>tw :silent! %s/\s\+$//<CR>:retab<CR>

command! -bang -nargs=? QFix call QFixToggle(<bang>0)
function! QFixToggle(forced)
  if exists("g:qfix_win") && a:forced == 0
    cclose
    unlet g:qfix_win
  else
    copen 10
    let g:qfix_win = bufnr("$")
  endif
endfunction
nnoremap <silent><leader>q :QFix<CR>

command! -bang -nargs=? LList call LListToggle(<bang>0)
function! LListToggle(forced)
  if exists("g:llist_win") && a:forced == 0
    lclose
    unlet g:llist_win
  else
    lopen 10
    let g:llist_win = bufnr("$")
  endif
endfunction
nnoremap <silent><leader>l :LList<CR>

" Next/prev from unimpaired.vim {{{
" [b, ]b, [B, ]B       :bprev, :bnext, :bfirst, :blast
" [l, ]l, [L, ]L       :lprev, :lnext, :lfirst, :llast
" [q, ]q, [Q, ]Q       :cprev, :cnext, :cfirst, :clast
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
" Set markdown as default syntax
let g:vimwiki_list = [{'path': '~/vimwiki/',
                      \ 'syntax': 'markdown', 'ext': '.markdown'}]

let g:vimwiki_folding = 'custom'
augroup VimrcAuGroup
  autocmd!
  autocmd FileType vimwiki setlocal foldmethod=expr |
    \ setlocal foldenable | set foldexpr=MarkdownLevel()
augroup END

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
  autocmd FileType pollen setlocal ts=2 sts=2 sw=2

  autocmd FileType pollen inoremap <C-l> λ
  autocmd FileType pollen inoremap <C-e> ◊
augroup END

augroup racket
  autocmd!
  autocmd FileType pollen inoremap <C-l> λ
  autocmd FileType pollen inoremap <C-e> ◊
augroup END

" }}}
" Rust {{{

augroup rustgroup
  autocmd!
  autocmd BufWrite *.rs :Autoformat
  autocmd Filetype rust nnoremap <leader>c :Dispatch cargo clippy --all-targets --all-features -- -D warnings<CR>
  autocmd FileType rust let b:dispatch = 'cargo check'
augroup END
" }}}
" Elixir {{{
let g:mix_format_on_save = 1
let g:mix_format_options = '--check-equivalent'
let g:mix_format_silent_errors = 1
" }}}
" Web {{{

augroup web
  autocmd!
  autocmd Filetype html setlocal ts=2 sts=2 sw=2
  autocmd Filetype javascript setlocal ts=2 sts=2 sw=2
  autocmd Filetype json setlocal ts=2 sts=2 sw=2
  autocmd FileType typescriptreact let b:dispatch = 'yarn next build'
augroup END

" }}}
" Vim {{{

augroup vim_filetype
  autocmd!
  autocmd Filetype vim setlocal foldmethod=marker
  autocmd Filetype vim setlocal nofoldenable
  autocmd Filetype vim setlocal ts=2 sts=2 sw=2
  "autocmd FileType vim :normal zM
augroup END

" }}}
" XML {{{
" FIXME can we make this work on visual selection?
command! FormatXML :%!python3 -c "import xml.dom.minidom, sys; print(xml.dom.minidom.parse(sys.stdin).toprettyxml())"
" }}}
" json {{{
" FIXME can we make this work on visual selection?
command! FormatJSON :%!python3 -m json.tool
" }}}
" }}}
"Firenvim {{{
if exists('g:started_by_firenvim') && g:started_by_firenvim
  let g:firenvim_config = { 
      \ 'localSettings': {
          \ '.*': {
              \ 'cmdline': 'firenvim',
              \ 'priority': 0,
              \ 'selector': 'textarea',
              \ 'takeover': 'never',
          \ },
      \ }
  \ }

  set laststatus=0 nonumber noruler noshowcmd
  set background=light

  augroup firenvim
    autocmd!
    autocmd BufEnter *.txt setlocal filetype=markdown
  augroup END
endif
"}}}
" Lua configs {{{
lua require("generic")
lua require("lsp_config")
lua require("treesitter_config")
lua require("telescope")
"}}}
"Git {{{
nnoremap gs :Git<CR>
nnoremap g<space> :Git 
nnoremap gll :Flogsplit<CR>
nnoremap glf :Flogsplit -path=%<CR>
xnoremap glf :Flogsplit -- --no-patch<CR>
nnoremap <silent> <leader>fc :Commits<CR>

" Fugitive Conflict Resolution
nnoremap gds :Gdiffsplit!<CR>
nnoremap gdh :diffget //2<CR>
nnoremap gdl :diffget //3<CR>

let g:git_messenger_no_default_mappings = v:true
nnoremap g? <Plug>(git-messenger)

" Toggle --no-patch for all commits.
" Toggling for specific commits isn't supported, but maybe will be later.
" https://github.com/rbong/vim-flog/issues/39
function ToggleFlogNoPatch() abort
    let l:state = flog#get_state()
    let l:raw_args = l:state['raw_args']
    if empty(l:raw_args)
        let l:raw_args = ''
    endif
    let l:split_raw_args = split(l:raw_args)
    let l:no_patch_arg_index = index(l:split_raw_args, '--no-patch')
    if l:no_patch_arg_index >= 0
        if len(l:split_raw_args) == 1
            let l:raw_args = v:null
        else
            call remove(l:split_raw_args, l:no_patch_arg_index)
            let l:raw_args = join(l:split_raw_args, ' ')
        endif
    else
        let l:raw_args .= ' --no-patch'
    endif
    let l:state['raw_args'] = l:raw_args
    call flog#populate_graph_buffer()
endfunction
autocmd FileType floggraph nnoremap <buffer> <silent> = :<C-U>call ToggleFlogNoPatch()<CR>

"}}}
"{{{ cheat40
" Don't use default cheat sheet
let g:cheat40_use_default = 0
" Hide folds
let g:cheat40_foldlevel = 0
"}}}
"{{{ hexokinase
let g:Hexokinase_highlighters = [ 'virtual' ]
" Filetypes to match
" Because it's async maybe this is completely unnecessary...
let g:Hexokinase_ftEnabled = ['css', 'html', 'javascript', 'scss', 'markdown', 'vimwiki', 'json']
" Patterns to match
let g:Hexokinase_optInPatterns = 'full_hex,triple_hex,rgb,rgba,hsl,hsla'
" Filetype specific patterns to match
let g:Hexokinase_ftOptInPatterns = {
\     'css': 'full_hex,triple_hex,rgb,rgba,hsl,hsla,colour_names',
\     'scss': 'full_hex,triple_hex,rgb,rgba,hsl,hsla,colour_names',
\     'html': 'full_hex,triple_hex,rgb,rgba,hsl,hsla,colour_names'
\ }
"}}}
"{{{ indent_blankline
" Prefer treesitter if available
let g:indent_blankline_use_treesitter = v:true
" Ignore some unnecessary lines
let g:indent_blankline_show_trailing_blankline_indent = v:false
"}}}
"{{{ fern
let g:fern#renderer = "nerdfont"

function! s:init_fern() abort
  " Expand and collapse with arrows
  nmap <buffer><nowait> <right> <Plug>(fern-action-expand)
  nmap <buffer><nowait> <left> <Plug>(fern-action-collapse)

  " Don't overwrite window switching
  nnoremap <buffer><nowait> <C-h> <c-w>h
  nnoremap <buffer><nowait> <C-l> <c-w>l
  nnoremap <buffer><nowait><silent> <M-l><Plug>(fern-action-redraw)
endfunction

augroup my-fern
  autocmd! *
  autocmd FileType fern call s:init_fern()
  autocmd FileType fern call glyph_palette#apply()
augroup END

"}}}
" FZF {{{
" Customize fzf colors to match your color scheme
" - fzf#wrap translates this to a set of `--color` options
let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'border':  ['fg', 'Ignore'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }

" Default fzf layout
" - Popup window
let g:fzf_layout = { 'window': '-tabnew' }
" }}}
" Sneak {{{
" Use seank as a minimalist alternative to EasyMotion
"let g:sneak#label = 1
" }}}
" vim:set sw=2 sts=2:
