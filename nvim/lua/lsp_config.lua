local map = function(type, key, value)
    vim.fn.nvim_buf_set_keymap(0,type,key,value,{noremap = true, silent = true});
end

local autocmd = function(event, pat, cmd)
    vim.cmd(table.concat({'autocmd', event, pat, cmd}, ' '))
end

local custom_attach = function(client)
    require'completion'.on_attach(client)

    map('n','gD','<cmd>lua vim.lsp.buf.declaration()<CR>')
    map('n','gd','<cmd>lua vim.lsp.buf.definition()<CR>')
    map('n','gr','<cmd>lua vim.lsp.buf.references()<CR>')
    map('n','gi','<cmd>lua vim.lsp.buf.implementation()<CR>')
    map('n','gt','<cmd>lua vim.lsp.buf.type_definition()<CR>')
    map('n','K','<cmd>lua vim.lsp.buf.hover()<CR>')
    map('n','<leader>ar','<cmd>lua vim.lsp.buf.rename()<CR>')
    map('n','<leader>ai','<cmd>lua vim.lsp.buf.incoming_calls()<CR>')
    map('n','<leader>ao','<cmd>lua vim.lsp.buf.outgoing_calls()<CR>')
    map('n','<leader>gw','<cmd>lua vim.lsp.buf.document_symbol()<CR>')
    map('n','<leader>gW','<cmd>lua vim.lsp.buf.workspace_symbol()<CR>')
    --map('n','gs','<cmd>lua vim.lsp.buf.signature_help()<CR>')
    --map('n','<leader>af','<cmd>lua vim.lsp.buf.code_action()<CR>')
    --map('n','<leader>=', '<cmd>lua vim.lsp.buf.formatting()<CR>')

    -- Goto previous/next diagnostic warning/error
    map('n',']d','<cmd>lua vim.lsp.diagnostic.goto_next()<CR>')
    map('n','[d','<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>')

    -- Omnicompletion support
    vim.api.nvim_command('setlocal omnifunc=v:lua.vim.lsp.omnifunc')

    -- Set completeopt to have a better completion experience
    -- together with LSP. Otherwise it automatically insert stuff when typing.
    -- :help completeopt
    -- menuone: popup even when there's only one match
    -- noinsert: Do not insert text until a selection is made
    -- noselect: Do not select, force user to select one from the menu
    vim.api.nvim_command('setlocal completeopt=menuone,noinsert,noselect')

    -- Show diagnostics on hover
    vim.api.nvim_command('setlocal updatetime=300')
    autocmd('Cursorhold', '*', 'lua vim.lsp.diagnostic.show_line_diagnostics()')

    -- Enable type inlay hints
    -- autocmd('CursorMoved,InsertLeave,BufEnter,BufWinEnter,TabEnter,BufWritePost',
    --         '*',
    --         [[lua require'lsp_extensions'.inlay_hints{
    --             prefix = '',
    --             highlight = 'Comment'
    --         }]])
end

require'lspconfig'.rust_analyzer.setup({
    on_attach = custom_attach;
    settings = {
        ["rust-analyzer"] = {
            diagnostics = {
                -- Disables 'proc macro `Serialize` not expanded and similar
                -- https://github.com/rust-analyzer/rust-analyzer/pull/6645
                disabled = {"unresolved-proc-macro"}
            }
        }
    }
})
require'lspconfig'.elixirls.setup({
    on_attach = custom_attach;
    cmd = { os.getenv("ELIXIR_LS_LANGUAGE_SERVER") };
})

require'lspconfig'.tsserver.setup{
    on_attach = custom_attach;
}

vim.api.nvim_command('command! LspStop :lua vim.lsp.stop_client(vim.lsp.get_active_clients())<CR>')
vim.api.nvim_command('command! LspStarted :lua print(vim.inspect(vim.lsp.buf_get_clients()))<CR>')

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, {
    virtual_text = true,

    -- This is similar to:
    -- let g:diagnostic_show_sign = 1
    -- To configure sign display,
    --  see: ":help vim.lsp.diagnostic.set_signs()"
    signs = true,

    -- Don't show diagnostics while in insert mode
    update_in_insert = false,
  }
)

