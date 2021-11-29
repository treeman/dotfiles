require('gitsigns').setup {
    signs = {
        add          = {text = '+'},
        change       = {text = '~'},
        delete       = {text = '-'},
        topdelete    = {text = '-'},
        changedelete = {text = '~'},
    },
}

-- Setup nvim-cmp.
local cmp = require'cmp'

cmp.setup({
    snippet = {
        expand = function(args)
            vim.fn["vsnip#anonymous"](args.body)
        end,
    },
    mapping = {
        ['<C-y>'] = cmp.config.disable, -- Specify `cmp.config.disable` to remove default mappings.
        ['<C-d>'] = cmp.config.disable, -- Specify `cmp.config.disable` to remove default mappings.
        ['<C-f>'] = cmp.config.disable, -- Specify `cmp.config.disable` to remove default mappings.
        ['<PgUp>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
        ['<PgDn>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
        ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),
        ['<C-e>'] = cmp.mapping({
            i = cmp.mapping.abort(),
            c = cmp.mapping.close(),
        }),
        ['<Tab>'] = cmp.mapping.confirm({ select = true }),
    },
    sources = {
        { name = 'nvim_lsp' },
        { name = 'vsnip' },
        { name = 'buffer' },
        { name = 'spell' },
        { name = 'calc' },
        { name = 'path' },
    }
})

-- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline('/', {
    sources = {
        { name = 'buffer' }
    }
})

local actions = require('telescope.actions')
require('telescope').setup {
    defaults = {
        file_ignore_patterns = {"node_modules"},
        mappings = {
            i = {
                ["<esc>"] = actions.close
            },
        },
    }
}

require("cheatsheet").setup({
    bundled_cheatsheets = false,
    bundled_plugin_cheatsheets = false,
})
