" Random unsorted plugins
"https://www.reddit.com/r/vim/comments/gbhvlo/what_am_i_missing_by_not_using_fzf/
"https://www.reddit.com/r/vim/comments/8riofp/airlinelightline_not_useful/
"https://www.reddit.com/r/vim/comments/775n4o/is_there_any_way_to_switch_statusbar_color_or/
"https://www.reddit.com/r/neovim/comments/gjz5cx/whats_a_plugin_that_does_something_you_didnt/
"https://www.reddit.com/r/vim/comments/gk53u1/just_discovered_ca_and_cx/
"https://www.reddit.com/r/vim/comments/gjz27p/whats_a_plugin_that_does_something_you_didnt/
"https://www.reddit.com/r/vim/comments/gib54k/anyone_has_a_copy_of_custom_vim_refactorings/
"https://www.reddit.com/r/vim/comments/a0q8dv/id_like_to_update_to_modern_vim_practices_what/
"https://www.reddit.com/r/neovim/comments/g5uo37/supercharge_your_vim_with_fzf_ripgrep/
"https://www.reddit.com/r/vim/comments/gfouey/markdown_mode_with_collapsible_blocks_and/
"https://stackoverflow.com/questions/13337618/how-to-use-customized-key-to-start-visual-block-selection-in-vim
"https://blog.usejournal.com/a-detailed-guide-to-writing-your-first-neovim-plugin-in-rust-a81604c606b1?gi=2c1f7e07ec18
"https://medium.com/@caleb89taylor/a-guide-to-modern-web-development-with-neo-vim-333f7efbf8e2
"https://github.com/Shougo/denite.nvim/blob/master/README.md
"https://dev.to/drmason13/configure-neovim-for-rust-development-1fjn
"https://kodi.wiki/view/Add-on:VimCasts
"https://github.com/lambdalisue/gina.vim/blob/master/README.md
"https://github.com/jreybert/vimagit/blob/master/README.md
"https://github.com/tpope/vim-dispatch/blob/master/README.markdown

" nvim-lsp for rust
"https://dev.to/drmason13/configure-neovim-for-rust-development-1fjn

" Plugins to check
" Comment stuff out
" https://github.com/tpope/vim-commentary
" Git
" https://github.com/tpope/vim-fugitive
" Surrounding things
" https://github.com/tpope/vim-surround
" Supposedly good mappings for things
" https://github.com/tpope/vim-unimpaired
" Launch async things
" https://github.com/tpope/vim-dispatch
" Show git changes in gutter
" https://github.com/airblade/vim-gitgutter

filetype plugin indent on

" Kill annoying beep sound?
set visualbell


" UI
set laststatus=2 " always show the status line
set linespace=0 " don't insert any extra pixel lines between rows
set report=0 " tell us when anything is changed via :...
set shortmess=aOstT " shortens messages to aviod 'perss a key' prompt
set ruler " always show current positions along the bottom
set showcmd " show the command being typed
"set completeopt= "don't use a pop up menu for completions


" Bind buffert toggling to f1/f2
noremap <F1> :bprev!<CR>
noremap <F2> :bnext!<CR>

" Jump to previously opened buffer
noremap <F3> :b#<CR>

" trim trailing spaces and convert tabs to spaces
map <F5> :silent! %s/\s\+$//<CR>:retab<CR>

" Spell checking
map <F10> :set spell! spell?<CR>

" Pretty format json:
" :%!python -m json.tool

" Open url in firefox.
function! HandleURL()
  let s:uri = matchstr(getline("."), '[a-z]*:\/\/[^ >,;]*')
  echo s:uri
  if s:uri != ""
    silent exec "!firefox '".s:uri."'"
  else
    echo "No URI found in line."
  endif
endfunction
map <leader>u :call HandleURL()<cr>


