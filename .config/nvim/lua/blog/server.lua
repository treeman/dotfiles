-- :BlogStart
-- :BlogStop
-- :BlogRestart
-- :BlogPreview
--
-- Should start `blog watch` in terminal when opening a dj or md file in our blog
-- Should show connection + server status in statusbar
-- Should cwd to blog_path when loading a file

local path = require("blog.path")
local utils = require("util.utils")
local nio = require("nio")

M = {}

M.is_connected = function()
	return M._blog_conn ~= nil
end

-- Event handling

M._send_msg = function(msg)
	local conn = M._blog_conn
	if conn then
		vim.fn.chansend(conn, vim.fn.json_encode(msg))
		-- Watcher tries to read lines so we need to terminate the message with a newline
		vim.fn.chansend(conn, "\n")
	else
		print("Error: trying to send a message without being connected")
	end
end

M._gen_message_id = function()
	local message_id = M._blog_message_id or 0
	M._blog_message_id = message_id + 1
	return message_id
end

M.call = function(msg, cb)
	if not M.is_connected() then
		print("Not connected")
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

			nio.scheduler()

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
		print("Not connected")
		return
	end

	nio.run(function()
		M._send_msg(msg)
	end)
end

-- This fun little thing tries to connect to my blogging
-- watch server and sends it document positions on move.
M.update_position = function()
	M.cast({
		id = "CursorMoved",
		-- context = vim.fn.getline("."),
		linenum = vim.fn.line("."),
		linecount = vim.fn.line("$"),
		column = vim.fn.col("."),
		path = vim.fn.expand("%:p"),
	})
end

-- FIXME should request diagnostics when file is read too
M._add_diagnostics = function(msg)
	for _, buf in ipairs(utils.list_buffers()) do
		local buf_name = vim.api.nvim_buf_get_name(0)
		local buf_diagnostics = msg[buf_name]

		if buf_diagnostics then
			local diagnostics = {}
			for _, d in ipairs(buf_diagnostics) do
				table.insert(diagnostics, {
					lnum = d.linenum,
					end_lnum = d.end_linenum,
					col = d.column,
					end_col = d.end_column,
					message = d.message,
					severity = vim.diagnostic.severity.WARN,
				})
			end

			vim.diagnostic.set(vim.api.nvim_create_namespace("blog"), buf, diagnostics)
		end
	end
end

-- Server management

M.start_server = function()
	if M._blog_job_id ~= nil then
		print("blog server already started")
		return
	end

	local buf = vim.api.nvim_create_buf(true, true)
	M._blog_job_bufnr = buf
	vim.api.nvim_buf_call(buf, function()
		M._blog_job_id = vim.fn.termopen("./blog watch", {
			cwd = path.blog_path,
		})
		print("blog server started")
	end)
end

M.stop_server = function()
	if M._blog_job_id == nil then
		print("blog server not started")
		return
	end

	print("Stopping:", M._blog_job_id)

	vim.fn.jobstop(M._blog_job_id)
	vim.api.nvim_buf_delete(M._blog_job_bufnr, { force = true })

	M._blog_job_bufnr = nil
	M._blog_job_id = nil

	print("blog server stopped")
end

-- Server connection management

M.close_connection = function()
	if M._blog_conn == nil then
		print("No blog connection to close")
		return
	end

	print("Closing existing blog connection")
	vim.fn.chanclose(M._blog_conn)
	M._blog_conn = nil
end

M.handle_reply = function(data)
	if #data == 1 and data[1] == "" then
		print("Blog connection closed")
		M.close_connection()
		return
	end

	local reply = vim.fn.json_decode(data)
	if not reply then
		return
	end

	if reply["message_id"] then
		local message_id = tostring(reply["message_id"])
		local messages = M._blog_messages or {}
		messages[message_id] = reply
		M._blog_messages = messages
	elseif reply.id == "Diagnostics" then
		M._add_diagnostics(reply.diagnostics)
	else
		print("Unknown message:")
		P(reply)
	end

	-- local diagnostic = {
	-- 	bufnr = ,
	-- 	lnum = ,
	-- 	end_lnum = ,
	-- 	col = ,
	-- 	end_col = ,
	-- 	severity = vim.diagnostic.severity.ERROR,
	-- 	message = ,
	-- }
end

M.try_connect = function()
	if M._blog_conn then
		print("Already connected")
		return true
	end

	print("Trying to establish connection...")
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
		print("Established blog connection")
		return true
	end

	print("Connection error:", vim.inspect(err))
	return false
end

M.establish_connection = function(ensure_server_started)
	ensure_server_started = ensure_server_started or true
	print("start server?", ensure_server_started)

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
	local _ = nio.run(function()
		-- TODO maybe do something more intelligent here?
		local attempt = 0
		while attempt < 10 do
			print("trying...", attempt)
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
