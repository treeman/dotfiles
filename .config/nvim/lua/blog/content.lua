local helpers = require("util.helpers")
local nio = require("nio")
local server = require("blog.server")

local M = {}

M.list_tags = function(cb)
  server.call({
    id = "ListTags",
  }, cb)
end

M.extract_title = function(file_path)
  local title = helpers.run_cmd({
    cmd = "rg",
    args = {
      "-INo",
      "^title = (.+)$",
      file_path,
    },
  })

  if not title then
    return nil
  end

  return title:match('title = "([^%"]+)"')
end

M.list_posts = function(draft, cb)
  nio.run(function()
    local subcmd
    if draft then
      subcmd = "list-drafts"
    else
      subcmd = "list-posts"
    end

    local output = helpers.run_cmd({
      cmd = "cargo",
      args = {
        "run",
        "-q",
        "--",
        "-q",
        subcmd,
      },
      cwd = "/home/tree/code/jonashietala",
    })

    nio.scheduler()
    local posts = vim.fn.json_decode(output)
    cb(posts)
  end)
end

-- M.cursor_info = function(cb)
--   local pos = vim.api.nvim_win_get_cursor(0)
--
--   server.call({
--     id = "CursorInfo",
--     linenum = pos[1] - 1,
--     column = pos[2],
--     path = vim.fn.expand("%:p"),
--   }, cb)
-- end

return M
