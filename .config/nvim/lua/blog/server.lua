-- :BlogStart
-- :BlogStop
-- :BlogRestart
-- :BlogPreview
--
-- Should start `blog watch` in terminal when opening a dj or md file in our blog
-- Should show connection + server status in statusbar
-- Should cwd to blog_path when loading a file

local path = require("blog.path")
local diagnostics = require("blog.diagnostics")
local nio = require("nio")
local log = require("plenary.log").new({
	plugin = "blog",
	level = "error",
})

M = {}

M.is_connected = function()
	return M._blog_conn ~= nil
end

M.has_server = function()
	return M._blog_job_id ~= nil
end

M.status = function()
	local is_connected = M.is_connected()
	local has_server = M.has_server()

	if has_server and is_connected then
		return "s"
	elseif has_server then
		return "Server but not connected"
	elseif is_connected then
		return "v"
	else
		return ""
	end
end

-- Event handling

M._send_msg = function(msg)
	local conn = M._blog_conn
	if conn then
		vim.fn.chansend(conn, vim.fn.json_encode(msg))
		-- Watcher tries to read lines so we need to terminate the message with a newline
		vim.fn.chansend(conn, "\n")
	else
		log.error("Trying to send a message without being connected")
	end
end

M._gen_message_id = function()
	local message_id = M._blog_message_id or 0
	M._blog_message_id = message_id + 1
	return message_id
end

M.call = function(msg, cb)
	if not M.is_connected() then
		return nil
	end

	nio.run(function()
		-- Create a unique message id for the call
		local msg_id = M._gen_message_id()
		msg["message_id"] = msg_id

		M._send_msg(msg)

		local msg_id_s = tostring(msg_id)

		-- Wait for response. 1 sec should be more than enough, otherwise we bail.
		local attempt = 0
		while attempt < 100 do
			if M._blog_messages then
				local reply = M._blog_messages[msg_id_s]
				if reply then
					M._blog_messages[msg_id_s] = nil
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

M.cast = function(msg)
	if not M.is_connected() then
		return
	end

	nio.run(function()
		M._send_msg(msg)
	end)
end

-- Server management

M.start_server = function()
	if M._blog_job_id ~= nil then
		return
	end

	local buf = vim.api.nvim_create_buf(true, true)
	M._blog_job_bufnr = buf
	vim.api.nvim_buf_call(buf, function()
		M._blog_job_id = vim.fn.termopen("./blog watch", {
			cwd = path.blog_path,
		})
	end)
end

M.stop_server = function()
	if M._blog_job_id == nil then
		return
	end

	vim.fn.jobstop(M._blog_job_id)
	vim.api.nvim_buf_delete(M._blog_job_bufnr, { force = true })

	M._blog_job_bufnr = nil
	M._blog_job_id = nil
end

-- Server connection management

M.close_connection = function()
	if M._blog_conn == nil then
		return
	end

	vim.fn.chanclose(M._blog_conn)
	M._blog_conn = nil
end

M.handle_reply = function(data)
	if #data == 1 and data[1] == "" then
		M.close_connection()
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

	if reply["message_id"] then
		local message_id = tostring(reply["message_id"])
		local messages = M._blog_messages or {}
		messages[message_id] = reply
		M._blog_messages = messages
	elseif reply.id == "Diagnostics" then
		diagnostics.add_diagnostics(reply.diagnostics)
	else
		log.debug("Unknown message:", vim.inspect(reply))
	end
end

M.try_connect = function()
	if M._blog_conn then
		return true
	end

	local status, err = pcall(function()
		M._blog_conn = vim.fn.sockconnect("tcp", "127.0.0.1:8082", {
			on_data = function(_, data, _)
				nio.run(function()
					M.handle_reply(data)
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

M.establish_connection = function(ensure_server_started)
	ensure_server_started = ensure_server_started or true

	-- To figure out if we have a server started somewhere
	-- (from another Neovim instance or from the command line)
	-- we first try to connect to it.
	if M.try_connect() then
		return
	end

	-- If that fails, try to start the blog server.
	if ensure_server_started then
		M.start_server()
	end

	-- Then try to reconnect, via a task to not block the UI.
	-- This isn't super smart, but it seems to work.
	local _ = nio.run(function()
		local attempt = 0
		while attempt < 10 do
			attempt = attempt + 1
			nio.sleep(1000)
			if M.try_connect() then
				return
			end
		end
	end)
end

M.start = function()
	M.start_server()
	M.establish_connection(false)
end

M.stop = function()
	M.close_connection()
	M.stop_server()
end

M.restart = function()
	M.stop()
	M.start()
end

M.reconnect = function()
	M.close_connection()
	M.try_connect()
end

M.preview = function() end

return M
