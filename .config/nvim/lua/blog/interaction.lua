local content = require("blog.content")
local server = require("blog.server")

local M = {}

M.goto_def = function()
  local pos = vim.api.nvim_win_get_cursor(0)

  server.call({
    id = "GotoDef",
    linenum = pos[1] - 1,
    column = pos[2],
    path = vim.fn.expand("%:p"),
  }, function(reply)
    if reply.path or reply.linenum then
      vim.cmd(":normal m'")
    end

    if reply.path then
      vim.cmd(":e" .. reply.path)
    end

    if reply.linenum then
      vim.api.nvim_win_set_cursor(0, { reply.linenum + 1, reply.column })
    end
  end)
end

local function element_docs(element)
  if not element then
    return nil
  end

  if element.type == "Link" then
    return {
      "<" .. element.link_ref.url .. ">",
    }
  elseif element.type then
    print("Unknown element: ", element.type)
    return vim.split(vim.inspect(element), "\n")
  end
end

M.hover = function()
  content.cursor_info(function(reply)
    local docs = element_docs(reply.element)
    if docs and #docs > 0 then
      local buf = vim.api.nvim_create_buf(false, true)
      vim.api.nvim_set_option_value("filetype", "djot", { buf = buf })
      vim.api.nvim_buf_set_lines(buf, 0, -1, false, docs)

      local height = #docs
      local width = 0

      for _, line in ipairs(docs) do
        local line_width = #line
        if line_width > width then
          width = line_width
        end
      end

      vim.b[0].blog_float_win = vim.api.nvim_open_win(buf, false, {
        relative = "cursor",
        width = width,
        height = height,
        col = 0,
        row = 1,
        focusable = false,
        style = "minimal",
        border = "none",
      })
    end
  end)
end

return M
