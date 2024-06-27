local diagnostics = require("blog.diagnostics")
local log = require("plenary.log").new({
  plugin = "blog",
  level = "error",
})
local nio = require("nio")
local path = require("blog.path")

local function is_connected()
  return vim.g.blog_conn ~= nil
end

local function is_blog_buf()
  return vim.b[0].blog_file
end

local function is_buf_connected()
  return is_connected() and is_blog_buf()
end

local function has_server()
  return vim.g.blog_job_id ~= nil
end

local function blog_status()
  if not vim.b[0].blog_file then
    return ""
  end

  local connected = is_connected()
  local server = has_server()

  if server and connected then
    return "s"
  elseif server then
    return "Server but not connected"
  elseif connected then
    return "c"
  else
    return ""
  end
end

-- Event handling

local function _send_msg(msg)
  local conn = vim.g.blog_conn
  if conn then
    vim.fn.chansend(conn, vim.fn.json_encode(msg))
    -- Watcher tries to read lines so we need to terminate the message with a newline
    vim.fn.chansend(conn, "\n")
  else
    log.error("Trying to send a message without being connected")
  end
end

local function _gen_message_id()
  local message_id = vim.g.blog_message_id or 0
  vim.g.blog_message_id = message_id + 1
  return message_id
end

local function call(msg, cb)
  if not is_connected() then
    return nil
  end

  nio.run(function()
    -- Create a unique message id for the call
    local msg_id = _gen_message_id()
    msg["message_id"] = msg_id

    _send_msg(msg)

    local msg_id_s = tostring(msg_id)

    -- Wait for response. 1 sec should be more than enough, otherwise we bail.
    local attempt = 0
    while attempt < 100 do
      if vim.g.blog_messages then
        local reply = vim.g.blog_messages[msg_id_s]
        if reply then
          vim.g.blog_messages[msg_id_s] = nil
          return reply
        end
      end

      attempt = attempt + 1
      nio.sleep(10)
    end

    -- Response timed out
    return false
  end, function(success, reply)
    if success and reply then
      cb(reply)
    end
  end)
end

local function cast(msg)
  if not is_connected() then
    return
  end

  nio.run(function()
    _send_msg(msg)
  end)
end

-- Server management

local function start_server()
  if vim.g.blog_job_id ~= nil then
    return
  end

  local buf = vim.api.nvim_create_buf(true, true)
  vim.g.blog_job_bufnr = buf
  vim.api.nvim_buf_call(buf, function()
    vim.g.blog_job_id = vim.fn.termopen("./blog watch", {
      cwd = path.blog_path,
    })
  end)
end

local function stop_server()
  if vim.g.blog_job_id == nil then
    return
  end

  vim.fn.jobstop(vim.g.blog_job_id)
  vim.api.nvim_buf_delete(vim.g.blog_job_bufnr, { force = true })

  vim.g.blog_job_bufnr = nil
  vim.g.blog_job_id = nil
end

-- Server connection management

local function close_connection()
  if vim.g.blog_conn == nil then
    return
  end

  vim.fn.chanclose(vim.g.blog_conn)
  vim.g.blog_conn = nil
end

local function handle_reply(data)
  if #data == 1 and data[1] == "" then
    close_connection()
    return
  end

  if #data == 0 then
    log.warn("Empty data received")
    return
  end

  local reply = vim.fn.json_decode(data[1])
  if not reply then
    return
  end

  -- vim.notify(vim.inspect(reply), vim.log.levels.ERROR)

  if reply["message_id"] then
    local message_id = tostring(reply["message_id"])
    local messages = vim.g.blog_messages or {}
    messages[message_id] = reply
    vim.g.blog_messages = messages
  elseif reply.id == "Diagnostics" then
    diagnostics.add_diagnostics(reply.diagnostics)
  else
    log.debug("Unknown message:", vim.inspect(reply))
  end
end

local function try_connect()
  if vim.g.blog_conn then
    return true
  end

  local status, err = pcall(function()
    vim.g.blog_conn = vim.fn.sockconnect("tcp", "127.0.0.1:8082", {
      on_data = function(_, data, _)
        nio.run(function()
          handle_reply(data)
        end)
      end,
    })
  end)

  if status then
    diagnostics.request_diagnostics_curr_buf()
    return true
  end

  log.debug("Connection error:", vim.inspect(err))
  return false
end

local function establish_connection(ensure_server_started)
  ensure_server_started = ensure_server_started or true

  -- To figure out if we have a server started somewhere
  -- (from another Neovim instance or from the command line)
  -- we first try to connect to it.
  if try_connect() then
    return
  end

  -- If that fails, try to start the blog server.
  if ensure_server_started then
    start_server()
  end

  -- Then try to reconnect, via a task to not block the UI.
  -- This isn't super smart, but it seems to work.
  local _ = nio.run(function()
    local attempt = 0
    while attempt < 10 do
      attempt = attempt + 1
      nio.sleep(1000)
      if try_connect() then
        return
      end
    end
  end)
end

local function start()
  start_server()
  establish_connection(false)
end

local function stop()
  close_connection()
  stop_server()
end

local function restart()
  stop()
  start()
end

local function reconnect()
  close_connection()
  try_connect()
end

-- Define export here because lib loading sometimes makes these nil...?
return {
  is_buf_connected = is_buf_connected,
  is_connected = is_connected,
  has_server = has_server,
  call = call,
  cast = cast,
  blog_status = blog_status,
  start = start,
  handle_reply = handle_reply,
  try_connect = try_connect,
  establish_connection = establish_connection,
  stop = stop,
  restart = restart,
  reconnect = reconnect,
}
