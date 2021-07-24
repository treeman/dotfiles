require('gitsigns').setup {
  signs = {
    add          = {text = '+'},
    change       = {text = '~'},
    delete       = {text = '-'},
    topdelete    = {text = '-'},
    changedelete = {text = '~'},
  },
}

require'compe'.setup {
  enabled = true;
  autocomplete = true;
  debug = false;
  min_length = 1;
  preselect = 'enable';
  throttle_time = 80;
  source_timeout = 200;
  incomplete_delay = 400;
  max_abbr_width = 100;
  max_kind_width = 100;
  max_menu_width = 100;
  documentation = true;

  source = {
    path = true;
    buffer = true;
    calc = true;
    spell = false;
    nvim_lsp = true;
    nvim_lua = true;
    vsnip = true;
  };
}

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

