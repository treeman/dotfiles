local M = {}

M.create_cmd = function(command, f, opts)
  opts = opts or {}
  vim.api.nvim_create_user_command(command, f, opts)
end

M.list_buffers = function()
  return vim.tbl_filter(function(buf)
    return vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_buf_get_option(buf, "buflisted")
  end, vim.api.nvim_list_bufs())
end

M.file_modified = function(path)
  local f = io.popen("stat -c %Y " .. path)
  if f then
    return tonumber(f:read())
  else
    return nil
  end
end

M.run_cmd = function(args)
  local nio = require("nio")
  local proc = nio.process.run(args)

  if not proc then
    return nil
  end

  local err = proc.stderr.read()
  if err and string.len(err) ~= 0 then
    vim.notify(
      "error while running command " .. vim.inspect(args) .. ": " .. err,
      vim.log.levels.ERROR
    )
    return nil
  end

  return proc.stdout.read()
end

M.list_files = function(path, cb)
  local nio = require("nio")
  nio.run(function()
    local output = M.run_cmd({
      cmd = "fd",
      args = {
        "-t",
        "f",
        "\\.",
        path,
      },
    })

    if not output then
      return
    end

    nio.scheduler()
    local files = vim.fn.split(output, "\n")
    cb(files)
  end)
end

return M
