local map = function(type, key, value)
    vim.fn.nvim_buf_set_keymap(0,type,key,value,{noremap = true, silent = true});
end

local autocmd = function(event, pat, cmd)
    vim.cmd(table.concat({'autocmd', event, pat, cmd}, ' '))
end

local custom_attach = function(client)
    print("LSP started.");
    require'completion'.on_attach(client)
    require'diagnostic'.on_attach(client)

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
    --map('n','<leader>ee','<cmd>lua vim.lsp.util.show_line_diagnostics()<CR>')

    -- Goto previous/next diagnostic warning/error
    map('n','[d','<cmd>PrevDiagnosticCycle<cr>')
    map('n',']d','<cmd>NextDiagnosticCycle<cr>')

    -- Visualize diagnostics
    vim.g.diagnostic_enable_virtual_text = 1
    vim.g.diagnostic_trimmed_virtual_text = '40'
    -- Don't show diagnostics while in insert mode
    vim.g.diagnostic_insert_delay = 1

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
    autocmd('Cursorhold', '*', 'lua vim.lsp.util.show_line_diagnostics()')

    -- Enable type inlay hints
    autocmd('CursorMoved,InsertLeave,BufEnter,BufWinEnter,TabEnter,BufWritePost',
            '*',
            [[lua require'lsp_extensions'.inlay_hints{
                prefix = '',
                highlight = 'Comment'
            }]])
end

require'nvim_lsp'.rust_analyzer.setup({ on_attach=custom_attach })

vim.api.nvim_command('command! LspStop :lua vim.lsp.stop_client(vim.lsp.get_active_clients())<CR>')
vim.api.nvim_command('command! LspStarted :lua print(vim.inspect(vim.lsp.buf_get_clients()))<CR>')

