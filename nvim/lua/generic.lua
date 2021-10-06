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
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<CR>'] = cmp.mapping.confirm({ select = true }),
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

