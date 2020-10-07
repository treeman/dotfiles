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
    map('n',']d','cmd>NextDiagnosticCycle<cr>')

    -- Visualize diagnostics
    vim.g.diagnostic_enable_virtual_text = 1
    vim.g.diagnostic_trimmed_virtual_text = '40'
    -- Don't show diagnostics while in insert mode
    vim.g.diagnostic_insert_delay = 1

    -- Omnicompletion support
    vim.api.nvim_command('setlocal omnifunc=v:lua.vim.lsp.omnifunc')

    -- Show diagnostics on hover
    vim.o.updatetime = 300
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
