local nio = require("nio")
local path = require("blog.path")
local server = require("blog.server")
local util = require("util")

local M = {}

M.list_tags = function(cb)
  server.call({
    id = "ListTags",
  }, cb)
end

M.extract_title = function(file_path)
  local title = util.run_cmd({
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

    local output = util.run_cmd({
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

return M
