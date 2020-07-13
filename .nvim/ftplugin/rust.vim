lua require'nvim_lsp'.rust_analyzer.setup{}

" Use LSP omni-completion in Rust files
setlocal omnifunc=v:lua.vim.lsp.omnifunc

" Not sure how to use clippy
"nnoremap <leader>c :!cargo clippy
" See :help lsp-buf
"
" This doesn't work at all, maybe because rust doesn't have a declaration
" context
" nnoremap gd    <cmd>lua vim.lsp.buf.declaration()<CR>
"
" This works, can use it on traits to see implementations
nnoremap <localleader>i    <cmd>lua vim.lsp.buf.implementation()<CR>
" Jump to def
nnoremap <localleader>d    <cmd>lua vim.lsp.buf.definition()<CR>
" Hover description
nnoremap <localleader>h    <cmd>lua vim.lsp.buf.hover()<CR>
" Display signature of functions etc
nnoremap <localleader>H    <cmd>lua vim.lsp.buf.signature_help()<CR>
" Goto type definition from variable
nnoremap <localleader>td   <cmd>lua vim.lsp.buf.type_definition()<CR>
" Goto usages of a type
nnoremap <localleader>r    <cmd>lua vim.lsp.buf.references()<CR>
" Go through symbols in a document (functions, definitions etc)
nnoremap <localleader>s    <cmd>lua vim.lsp.buf.document_symbol()<CR>
" Go through symbols in the workspace (takes a search query)
nnoremap <localleader>S    <cmd>lua vim.lsp.buf.workspace_symbol()<CR>

