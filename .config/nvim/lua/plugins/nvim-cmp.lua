local cmp = require("cmp")
local context = require("cmp.config.context")
local luasnip = require("luasnip")

local function in_comment()
  return context.in_treesitter_capture("comment") or context.in_syntax_group("Comment")
end

local function in_string()
  return context.in_treesitter_capture("string") or context.in_syntax_group("String")
end

local function in_spell()
  return context.in_treesitter_capture("spell")
end

local function disallowed_buftype()
  local buftype = vim.bo.buftype
  local disallowed = {
    "prompt",
    "nofile",
  }
  for _, v in pairs(disallowed) do
    if buftype == v then
      return true
    end
  end
  return false
end

local blog_types = {
  BrokenLink = { rank = 0, symbol = "󰌺" },
  LinkDef = { rank = 1, symbol = "󰌹" },
  Heading = { rank = 2, symbol = "" },
  Post = { rank = 3, symbol = "󱚌" },
  Tag = { rank = 4, symbol = "󰓹" },
  Series = { rank = 5, symbol = "" },
  Standalone = { rank = 6, symbol = "󰂺" },
  Constant = { rank = 7, symbol = "" },
  Img = { rank = 8, symbol = "" },
  DivClass = { rank = 9, symbol = "" },
}

local function blog_compare(entry1, entry2)
  -- Only sort blog entries.
  if entry1.source.name ~= "blog" or entry2.source.name ~= "blog" then
    return nil
  end

  local item1 = entry1.completion_item
  local item2 = entry2.completion_item

  local rank1 = blog_types[item1.type].rank
  local rank2 = blog_types[item2.type].rank
  if rank1 < rank2 then
    return true
  elseif rank1 > rank2 then
    return false
  end

  if item1.type == "Img" then
    return item1.modified > item2.modified
  elseif item1.type == "Post" then
    return item1.created > item2.created
  elseif item1.type == "Series" then
    return item1.posts[1].created > item2.posts[1].created
  elseif item1.type == "Tag" then
    return #item1.posts > #item2.posts
  end

  return nil
end

local blog_format = function(entry, vim_item)
  local type = entry.completion_item.type
  vim_item.kind = blog_types[type].symbol .. " " .. type
  vim_item.menu = "[BLOG]"
  return vim_item
end

local function make_format()
  local lspkind_format = require("lspkind").cmp_format({
    mode = "symbol_text",
    ellipsis_char = "…",
    menu = {
      luasnip = "[SNIP]",
      nvim_lsp = "[LSP]",
      nvim_lua = "[LUA]",
      beancount = "[BEAN]",
      spell = "[SPELL]",
      treesitter = "[TREE]",
      async_path = "[PATH]",
      buffer = "[BUF]",
      calc = "[CALC]",
      ["vim-dadbod-completion"] = "[DB]",
    },
  })

  return function(entry, vim_item)
    if entry.source.name == "blog" then
      return blog_format(entry, vim_item)
    else
      return lspkind_format(entry, vim_item)
    end
  end
end

cmp.setup({
  enabled = function()
    return not in_comment() and not disallowed_buftype()
  end,
  mapping = {
    ["<C-n>"] = cmp.mapping(cmp.mapping.select_next_item(), { "i", "c" }),
    ["<C-p>"] = cmp.mapping(cmp.mapping.select_prev_item(), { "i", "c" }),
    ["<PgUp>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" }),
    ["<PgDn>"] = cmp.mapping(cmp.mapping.scroll_docs(4), { "i", "c" }),
    ["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
    ["<C-e>"] = cmp.mapping({
      i = cmp.mapping.abort(),
      c = cmp.mapping.close(),
    }),
    ["<Tab>"] = cmp.mapping.confirm({ select = true }),
  },
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  sources = {
    {
      name = "luasnip",
      group_index = 1,
      entry_filter = function()
        return not in_string()
      end,
    },
    {
      name = "blog",
      group_index = 1,
    },
    {
      name = "nvim_lsp",
      group_index = 2,
    },
    {
      name = "nvim_lua",
      group_index = 2,
    },
    -- {
    --   name = "vim-dadbod-completion",
    --   group_index = 2,
    -- },
    {
      name = "spell",
      option = {
        enable_in_context = in_spell,
      },
      group_index = 3,
    },
    {
      name = "treesitter",
      keyword_length = 3,
      group_index = 4,
    },
    {
      name = "async_path",
      group_index = 4,
    },
    {
      name = "calc",
      group_index = 4,
    },
    {
      name = "buffer",
      keyword_length = 3,
      group_index = 5,
    },
  },
  formatting = {
    format = make_format(),
  },
  sorting = {
    priority_weight = 2,
    comparators = {
      cmp.config.compare.offset,
      cmp.config.compare.exact,
      cmp.config.compare.score,
      cmp.config.compare.recently_used,
      cmp.config.compare.locality,
      blog_compare,
      cmp.config.compare.kind,
      cmp.config.compare.sort_text,
      cmp.config.compare.length,
      cmp.config.compare.order,
    },
  },
})

cmp.setup.cmdline("/", {
  sources = {
    { name = "buffer" },
  },
})

local autocmd = vim.api.nvim_create_autocmd

autocmd("FileType", {
  pattern = "mysql,plsql",
  callback = function()
    require("cmp").setup.buffer({ sources = { { name = "vim-dadbod-completion" } } })
  end,
})
