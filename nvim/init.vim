" Installation {{{
" Update plugins
" :PlugUpdate
"
" Update treesitter:
" :TSUpdate
"
" Dependencies:
" git, ripgrep, fd, bat
"
" rust-analyzer:
"   https://github.com/rust-analyzer/rust-analyzer
"   cargo xtask install --server
"
" elixir-ls:
"   https://github.com/elixir-lsp/elixir-ls.git
"   mix compile
"   mix elixir_ls.release -o release
"   Ensure that $ELIXIR_LS_LANGUAGE_SERVER points to release/language_server.sh
"
" }}}
" Future ideas and TODOs {{{
" Move out file specific into ftplugin
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
set completeopt=menuone " Popup completion menu even with only one option

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
" Personal wiki
Plug 'https://github.com/vimwiki/vimwiki.git', { 'branch': 'dev' }
" Avoid mistyping filenames, ask which file to open if file not find
Plug 'https://github.com/EinfachToll/DidYouMean.git'
" Easy way to comment things
Plug 'https://github.com/tomtom/tcomment_vim'
" For many more features see tcomment_vim
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
" These are just for highlighting specific files
Plug 'https://github.com/nathangrigg/vim-beancount'
Plug 'https://github.com/dag/vim-fish.git'
Plug 'https://github.com/rust-lang/rust.vim'
Plug 'https://github.com/cespare/vim-toml.git'
Plug 'https://github.com/hail2u/vim-css3-syntax.git'
Plug 'https://github.com/wlangstroth/vim-racket'
Plug 'https://github.com/otherjoel/vim-pollen.git'
Plug 'https://github.com/elixir-editors/vim-elixir.git'
" Automatically insert end in insert mode for some languages
Plug 'https://github.com/tpope/vim-endwise'
" Light statusbar
Plug 'https://github.com/itchyny/lightline.vim'
Plug 'shinchu/lightline-gruvbox.vim'
" Treesitter syntax highlighting
Plug 'nvim-treesitter/nvim-treesitter'
Plug 'romgrk/nvim-treesitter-context'
" Dispatch async things
Plug 'https://github.com/tpope/vim-dispatch'
" Maximize for windows
Plug 'https://github.com/szw/vim-maximizer'

" LSP support
" See doc :help lsp
" Collection of common configurations for the Nvim LSP client
Plug 'neovim/nvim-lspconfig'
" Extensions to built-in LSP, for example, providing type inlay hints
Plug 'tjdevries/lsp_extensions.nvim'
" Autocompletion framework for built-in LSP
Plug 'nvim-lua/completion-nvim'

" Autoformat for different languages
Plug 'Chiel92/vim-autoformat'

" Git plugins
Plug 'https://github.com/airblade/vim-gitgutter'
Plug 'https://github.com/tpope/vim-fugitive'
Plug 'https://github.com/rbong/vim-flog'
Plug 'https://github.com/rhysd/git-messenger.vim'

" nvim in Firefox
Plug 'https://github.com/glacambre/firenvim', { 'do': { _ -> firenvim#install(0) } }
" Peek registry contents, for easy use of " and @
Plug 'https://github.com/junegunn/vim-peekaboo'
" Preview colors
Plug 'https://github.com/chrisbra/Colorizer'
call plug#end()

"{{{ Plugins to check
" File handling plugins:
" https://bluz71.github.io/2017/05/21/vim-plugins-i-like.html#fernvim
" CHADTree
"
" Quick substitutions:
" https://github.com/svermeulen/vim-subversive
"
" Git plugins:
" https://github.com/jreybert/vimagit
" https://github.com/tpope/vim-rhubarb
" https://github.com/wfxr/forgit
"
" Phoenix:
" smathy/vim-pheonix
" vim-projectionist
"
" More fuzzy funding examples:
"https://github.com/junegunn/fzf/wiki/Examples-(vim)
"use enter key, CTRL-T, CTRL-X or CTRL-V to open selected files in the current window, in new tabs, in horizontal splits, or in vertical splits respectively.

" Random unsorted plugins
"https://www.reddit.com/r/vim/comments/gbhvlo/what_am_i_missing_by_not_using_fzf/
"https://www.reddit.com/r/vim/comments/8riofp/airlinelightline_not_useful/
"https://www.reddit.com/r/vim/comments/775n4o/is_there_any_way_to_switch_statusbar_color_or/
"https://www.reddit.com/r/neovim/comments/gjz5cx/whats_a_plugin_that_does_something_you_didnt/
"https://www.reddit.com/r/vim/comments/gk53u1/just_discovered_ca_and_cx/
"https://www.reddit.com/r/vim/comments/gjz27p/whats_a_plugin_that_does_something_you_didnt/
"https://www.reddit.com/r/vim/comments/gib54k/anyone_has_a_copy_of_custom_vim_refactorings/
"https://www.reddit.com/r/vim/comments/a0q8dv/id_like_to_update_to_modern_vim_practices_what/
"https://stackoverflow.com/questions/13337618/how-to-use-customized-key-to-start-visual-block-selection-in-vim
"https://blog.usejournal.com/a-detailed-guide-to-writing-your-first-neovim-plugin-in-rust-a81604c606b1?gi=2c1f7e07ec18
"https://medium.com/@caleb89taylor/a-guide-to-modern-web-development-with-neo-vim-333f7efbf8e2
"https://github.com/Shougo/denite.nvim/blob/master/README.md
"https://dev.to/drmason13/configure-neovim-for-rust-development-1fjn
"https://kodi.wiki/view/Add-on:VimCasts
"https://github.com/lambdalisue/gina.vim/blob/master/README.md
"https://medium.com/@huntie/10-essential-vim-plugins-for-2018-39957190b7a9
"
" Better f/F
" https://github.com/rhysd/clever-f.vim
"
" LSP diagnostics:
" https://www.reddit.com/r/neovim/comments/jt9tqm/new_builtin_lsp_diagnostics_module/
" But requires us to update neovim
"}}}
"{{{ cheat40
" Don't use default cheat sheet
let g:cheat40_use_default = 0
" Hide folds
let g:cheat40_foldlevel = 0
"}}}
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
\ }

" These functions truncates away a bunch of stuff when width is smaller
function! LightlineGitStatus()
  if winwidth(0) <= 95
    return ""
  endif

  let [a,m,r] = GitGutterGetHunkSummary()

  let info = []
  if a != 0
    call add(info, printf('+%d', a))
  end
  if m != 0
    call add(info, printf('~%d', m))
  end
  if r != 0
    call add(info, printf('-%d', r))
  end

  return join(info)
endfunction

function! LightlineFilename()
  return winwidth(0) > 95 ? expand('%:f') : expand('%:t')
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

" This is too verbose unfortunately
" function! LightlineTreesitter()
"   return winwidth(0) > 70 ? nvim_treesitter#statusline(40) : ''
" endfunction
" }}}
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

" Find files
nnoremap <silent> <leader>ff :Files<CR>
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
" Normal mode:
nnoremap <C-h> <c-w>h
nnoremap <C-j> <c-w>j
nnoremap <C-k> <c-w>k
nnoremap <C-l> <c-w>l
" Create splits with <C-w>v and <C-w>s, or :sp and :vs

let g:maximizer_set_default_mapping = 0
nnoremap <silent><C-w>o :MaximizerToggle<CR>

" Goto previous buffer
nnoremap <leader>b :edit #<CR>

" Command autocomplete from the given prefix
" cnoremap <C-n> <Down>
" cnoremap <C-p> <Up>
" Maybe consider remapping up/down to C-n/C-p I guess...
" cnoremap <Down> <nop>
" cnoremap <Up> <nop>

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
  autocmd Filetype rust nnoremap <leader>c :Dispatch cargo clippy<CR>
  autocmd FileType rust let b:dispatch = 'cargo check'
augroup END
" }}}
" Web {{{

augroup web
  autocmd!
  autocmd Filetype html setlocal ts=2 sts=2 sw=2
  autocmd Filetype javascript setlocal ts=2 sts=2 sw=2
  autocmd Filetype json setlocal ts=2 sts=2 sw=2
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
command! FormatJSON :%!python -m json.tool
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
              \ 'takeover': 'once',
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
"LSP {{{
lua require("lsp_config")
"}}}
" Treesitter{{{
lua require("treesitter_config")
"}}}
"Git {{{
" Jump between changed hunks
nnoremap ]c :GitGutterNextHun<CR>
nnoremap [c :GitGutterPrevHunk<CR>
" FIXME many more things we can do. See here:
" https://github.com/airblade/vim-gitgutter

nnoremap gs :Git<CR>
nnoremap g<space> :Git 
nnoremap gll :Flogsplit<CR>
nnoremap glf :Flogsplit -path=%<CR>
xnoremap glf :Flogsplit -- --no-patch<CR>
nnoremap <silent> <leader>fc :Commits<CR>

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
" vim:set sw=2 sts=2: