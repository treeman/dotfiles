local map = function(type, key, value)
    vim.api.nvim_buf_set_keymap(0,type,key,value,{noremap = true, silent = true});
end

local autocmd = function(event, pat, cmd)
    vim.cmd(table.concat({'autocmd', event, pat, cmd}, ' '))
end

local custom_attach = function(_)
    -- Different keyboard layouts on laptop and main computer
    for _, prefix in ipairs({'_', '-'}) do
        -- Most here go through telescope via the lsp-handlers plugin
        map('n',prefix .. 'D','<cmd>lua vim.lsp.buf.declaration()<CR>')
        map('n',prefix .. 'd','<cmd>lua vim.lsp.buf.definition()<CR>')
        map('n',prefix .. 'r','<cmd>lua vim.lsp.buf.references()<CR>')
        map('n',prefix .. 'i','<cmd>lua vim.lsp.buf.implementation()<CR>')
        map('n',prefix .. 't','<cmd>lua vim.lsp.buf.type_definition()<CR>')
        map('n',prefix .. 'h','<cmd>lua vim.lsp.buf.hover()<CR>')
        map('n',prefix .. 's','<cmd>lua vim.lsp.buf.signature_help()<CR>')
        map('n',prefix .. 'x','<cmd>lua vim.lsp.buf.code_action()<CR>')
        map('n',prefix .. 'l','<cmd>lua vim.lsp.diagnostic.show_line_diagnostics({ focusable = false })<CR>')
        map('n',prefix .. 'ar','<cmd>lua vim.lsp.buf.rename()<CR>')
        map('n',prefix .. 'I','<cmd>lua vim.lsp.buf.incoming_calls()<CR>')
        map('n',prefix .. 'O','<cmd>lua vim.lsp.buf.outgoing_calls()<CR>')
        map('n',prefix .. 'w','<cmd>lua vim.lsp.buf.document_symbol()<CR>')
        map('n',prefix .. 'W','<cmd>lua vim.lsp.buf.workspace_symbol()<CR>')
        map('n',prefix .. 'e','<cmd>:Telescope lsp_document_diagnostics<CR>')
        map('n',prefix .. 'E','<cmd>:Telescope lsp_workspace_diagnostics<CR>')
        -- map('n','<leader>=', '<cmd>lua vim.lsp.buf.formatting()<CR>')
    end

    -- Goto previous/next diagnostic warning/error
    map('n',']d','<cmd>lua vim.lsp.diagnostic.goto_next()<CR>')
    map('n','[d','<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>')

    -- Omnicompletion support
    vim.api.nvim_command('setlocal omnifunc=v:lua.vim.lsp.omnifunc')

    -- Show diagnostics on hover
    vim.api.nvim_command('setlocal updatetime=150')
    autocmd('Cursorhold', '*', 'lua vim.lsp.diagnostic.show_line_diagnostics({ focusable = false })')

    autocmd('Cursorhold', '*', "lua require'nvim-lightbulb'.update_lightbulb()")

    -- Enable type inlay hints
    autocmd('InsertLeave,BufEnter,BufWinEnter,TabEnter,BufWritePost', '*',
            "lua require'lsp_extensions'.inlay_hints{prefix = '', highlight = 'Comment'}")
end

-- config that activates keymaps and enables snippet support
local function make_config()
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities.textDocument.completion.completionItem.snippetSupport = true
  capabilities.textDocument.completion.completionItem.resolveSupport = {
      properties = {
          'documentation',
          'detail',
          'additionalTextEdits',
      }
  }
  return {
    capabilities = capabilities,
    on_attach = custom_attach,
  }
end

-- Configure lua language server for neovim development
local lua_settings = {
  Lua = {
    runtime = {
      -- LuaJIT in the case of Neovim
      version = 'LuaJIT',
      path = vim.split(package.path, ';'),
    },
    diagnostics = {
      -- Get the language server to recognize the `vim` global
      globals = {'vim'},
    },
    workspace = {
      -- Make the server aware of Neovim runtime files
      library = {
        [vim.fn.expand('$VIMRUNTIME/lua')] = true,
        [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true,
      },
    },
  }
}
-- Local rust_analyzer settings
local rust_settings = {
    ["rust-analyzer"] = {
        diagnostics = {
            -- Disables 'proc macro `Serialize` not expanded and similar
            -- https://github.com/rust-analyzer/rust-analyzer/pull/6645
            disabled = {"unresolved-proc-macro"}
        }
    }
}

-- lsp-install
local function setup_servers()
  require'lspinstall'.setup()

  -- get all installed servers
  local servers = require'lspinstall'.installed_servers()
  -- ... and add manually installed servers
  -- table.insert(servers, "clangd")
  -- These can also be installed with `LspInstall <server>`
  table.insert(servers, "rust_analyzer")
  table.insert(servers, "elixirls")
  table.insert(servers, "efm")

  for _, server in pairs(servers) do
    local config = make_config()

    -- language specific config
    if server == "lua" then
      config.settings = lua_settings
    end
    if server == "rust_analyzer" then
      config.settings = rust_settings
    end
    if server == "elixirls" then
      config.cmd = { os.getenv("ELIXIR_LS_LANGUAGE_SERVER") }
    end
    if server == "clangd" then
      config.filetypes = {"c", "cpp"}; -- we don't want objective-c and objective-cpp!
    end
    if server == "efm" then
      config.filetypes = {"elixir"};
    end

    require'lspconfig'[server].setup(config)
  end
end

setup_servers()

-- Automatically reload after `:LspInstall <server>` so we don't have to restart neovim
require'lspinstall'.post_install_hook = function ()
  setup_servers() -- reload installed servers
  vim.cmd("bufdo e") -- this triggers the FileType autocmd that starts the server
end


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

require('telescope').load_extension('lsp_handlers')

-- commented options are defaults
require('lspkind').init({
    with_text = true,
    symbol_map = {
      Text = '',
      Method = 'ƒ',
      Function = '',
      Constructor = '',
      Variable = '',
      Class = '',
      Interface = 'ﰮ',
      Module = '',
      Property = '',
      Unit = '',
      Value = '',
      Enum = '了',
      Keyword = '',
      Snippet = '﬌',
      Color = '',
      File = '',
      Folder = '',
      EnumMember = '',
      Constant = '',
      Struct = ''
    },
})

require("lsp-rooter").setup({
  -- Table of lsp clients to ignore by name
  ignore_lsp = {},
})
