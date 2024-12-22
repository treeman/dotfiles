--- @param trunc_width number trunctates component when screen width is less then trunc_width
--- @param trunc_len number truncates component to trunc_len number of chars
--- @param hide_width number hides component when window width is smaller then hide_width
--- @param no_ellipsis boolean whether to disable adding '...' at end after truncation
--- return function that can format the component accordingly
local function trunc(trunc_width, trunc_len, hide_width, no_ellipsis)
  return function(str)
    local win_width = vim.fn.winwidth(0)
    if hide_width and win_width < hide_width then
      return ""
    elseif trunc_width and trunc_len and win_width < trunc_width and #str > trunc_len then
      return str:sub(1, trunc_len) .. (no_ellipsis and "" or "...")
    end
    return str
  end
end

local function lsp_info()
  if vim.lsp.get_clients() == nil then
    return ""
  else
    local t = trunc(240, 80, 0, false)
    -- Remove `%` to workaround `illegal character` error
    local s = require("lsp-status").status():gsub("%%", "")
    return t(s)
  end
end

local function server_info()
  return require("blog.server").blog_status() .. lsp_info()
end

local function spell()
  if vim.opt.spell:get() then
    return table.concat(vim.opt.spelllang:get(), ",")
  else
    return ""
  end
end

-- Note that "0x%0B" is bugged, but works with the %b prefix
-- local charhex = "%b 0x%B"
-- Show location s column:row/total_rows
local location = "%c:%l/%L"

-- Theme is hard.
-- I dumped the "auto" theme and tried to adjust some things that bothered me a little.
-- NOTE there's a lualine theme in melange-nvim now, but I couldn't figure out
-- how to load it.
local colors = {
  a = {
    bg = "#292522",
    float = "#34302C",
    sel = "#403A36",
    ui = "#867462",
    com = "#C1A78E",
    fg = "#ECE1D7",
  },
  b = {
    red = "#D47766",
    yellow = "#EBC06D",
    green = "#85B695",
    cyan = "#89B3B6",
    blue = "#A3A9CE",
    magenta = "#CF9BC2",
  },
  c = {
    red = "#BD8183",
    yellow = "#E49B5D",
    green = "#78997A",
    cyan = "#7B9695",
    blue = "#7F91B2",
    magenta = "#B380B0",
  },
  d = {
    red = "#7D2A2F",
    yellow = "#8B7449",
    green = "#233524",
    cyan = "#253333",
    blue = "#273142",
    magenta = "#422741",
  },
}
local custom_theme = {
  normal = {
    a = { bg = "#463f3b", fg = "#96918e", gui = "bold" },
    b = { bg = "#2d2825", fg = "#7d7672" },
    c = { bg = "#393430", fg = colors.a.com },
  },
  inactive = {
    a = { bg = "#463f3b", fg = "#96918e", gui = "bold" },
    b = { bg = "#2d2825", fg = "#7d7672" },
    c = { bg = "#393430", fg = colors.a.ui },
  },
  command = {
    a = { bg = colors.c.green, fg = "#2d2825", gui = "bold" },
    b = { bg = "#2d2825", fg = colors.c.green },
    c = { bg = "#393430", fg = colors.a.com },
  },
  terminal = {
    a = { bg = colors.c.cyan, fg = "#2d2825", gui = "bold" },
    b = { bg = "#2d2825", fg = colors.c.cyan },
    c = { bg = "#393430", fg = colors.a.com },
  },
  insert = {
    a = { bg = colors.c.cyan, fg = "#2d2825", gui = "bold" },
    b = { bg = "#2d2825", fg = colors.c.cyan },
    c = { bg = "#393430", fg = colors.a.com },
  },
  replace = {
    a = { bg = colors.c.magenta, fg = "#2d2825", gui = "bold" },
    b = { bg = "#2d2825", fg = colors.c.magenta },
    c = { bg = "#393430", fg = colors.a.com },
  },
  visual = {
    a = { bg = colors.c.yellow, fg = "#2d2825", gui = "bold" },
    b = { bg = "#2d2825", fg = colors.c.yellow },
    c = { bg = "#393430", fg = colors.a.com },
  },
}

local filename = {
  "filename",
  path = 1,
}

local function config()
  local lsp_status = require("lsp-status")
  local lualine = require("lualine")

  lsp_status.config({
    current_function = false,
    show_filename = false,
    diagnostics = false,
  })
  lsp_status.register_progress()

  lualine.setup({
    options = {
      theme = custom_theme,
      -- TODO
      -- theme = require("melange-nvim/lualine/themes/melange"),
      -- component_separators = { left = "", right = "" },
    },
    sections = {
      -- See current window width using :echo winwidth(0)
      -- Laptop
      -- full: 213
      -- half: 106
      -- third: 71
      lualine_a = { "mode" },
      lualine_b = {
        { "branch",      fmt = trunc(200, 20, 80, true) },
        { "diff",        fmt = trunc(80, 80, 80, true) },
        { "diagnostics", fmt = trunc(80, 80, 80, true) },
      },
      lualine_c = { filename },
      lualine_x = { { server_info, fmt = trunc(120, 10, 60, true) } },
      lualine_y = {
        { spell,      fmt = trunc(120, 120, 120, true) },
        { "encoding", fmt = trunc(120, 120, 120, true) },
        { "filetype", fmt = trunc(80, 80, 80, true) },
      },
      lualine_z = { location },
    },
    inactive_sections = {
      lualine_a = {},
      lualine_b = {},
      lualine_c = { filename },
      lualine_x = {},
      lualine_y = {},
      lualine_z = {},
    },
  })
end

return {
  "nvim-lualine/lualine.nvim",
  dependencies = {
    "nvim-tree/nvim-web-devicons",
    "nvim-lua/lsp-status.nvim",
    "savq/melange-nvim",
  },
  config = config,
  lazy = false,
}
