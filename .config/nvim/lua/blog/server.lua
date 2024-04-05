-- :BlogStart
-- :BlogStop
-- :BlogRestart
-- :BlogPreview
--
-- Should start `blog watch` in terminal when opening a dj or md file in our blog
-- Should show connection + server status in statusbar
-- Should cwd to blog_path when loading a file

local nio = require("nio")

local blog_path = "/home/tree/code/jonashietala/"

local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup

P = function(v)
	print(vim.inspect(v))
	return v
end

-- Event handling

local function send_msg(msg)
	local conn = vim.g.blog_conn
	vim.fn.chansend(conn, vim.fn.json_encode(msg))
	-- Watcher tries to read lines so we need to terminate the message with a newline
	vim.fn.chansend(conn, "\n")
end

local function gen_message_id()
	local message_id = vim.g.blog_message_id
	vim.g.blog_message_id = vim.g.blog_message_id + 1
	return message_id
end

local function call(msg, cb)
	if not vim.g.blog_conn then
		print("Not connected")
		return nil
	end

	nio.run(function()
		-- Create a unique message id for the call
		if not vim.g.blog_message_id then
			vim.g.blog_message_id = 0
		end

		local msg_id = gen_message_id()
		msg["message_id"] = msg_id

		send_msg(msg)

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

			nio.scheduler()

			attempt = attempt + 1
			nio.sleep(10)
		end

		-- Response timed out
		return false
	end, cb)
end

local function cast(msg)
	if not vim.g.blog_conn then
		print("Not connected")
		return
	end

	nio.run(function()
		send_msg(msg)
	end)
end

-- This fun little thing tries to connect to my blogging
-- watch server and sends it document positions on move.
local function update_position()
	cast({
		id = "CursorMoved",
		-- context = vim.fn.getline("."),
		linenum = vim.fn.line("."),
		linecount = vim.fn.line("$"),
		column = vim.fn.col("."),
		path = vim.fn.expand("%:p"),
	})
end

-- Server management

local function start_server()
	if vim.g.blog_job_id ~= nil then
		print("blog server already started")
		return
	end

	local buf = vim.api.nvim_create_buf(true, true)
	vim.g.blog_job_buf = buf
	vim.api.nvim_buf_call(buf, function()
		vim.g.blog_job_id = vim.fn.termopen("./blog watch", {
			cwd = blog_path,
		})
		print("blog server started")
	end)
end

local function stop_server()
	if vim.g.blog_job_id == nil then
		print("blog server not started")
		return
	end

	print("Stopping:", vim.g.blog_job_id)

	vim.fn.jobstop(vim.g.blog_job_id)
	vim.api.nvim_buf_delete(vim.g.blog_job_buf, { force = true })

	vim.g.blog_job_buf = nil
	vim.g.blog_job_id = nil

	print("blog server stopped")
end

-- Server connection management

local blog_group = augroup("blog", { clear = true })

local function close_connection()
	if vim.g.blog_conn == nil then
		print("No blog connection to close")
		return
	end

	print("Closing existing blog connection")
	vim.fn.chanclose(vim.g.blog_conn)
	vim.g.blog_conn = nil
end

local function handle_reply(data)
	if table.getn(data) == 1 and data[1] == "" then
		print("Blog connection closed")
		close_connection()
		return
	end

	local reply = vim.fn.json_decode(data)
	if reply and reply["message_id"] then
		local message_id = tostring(reply["message_id"])
		local messages = vim.g.blog_messages or {}
		messages[message_id] = reply
		vim.g.blog_messages = messages
	end
end

local function try_connect()
	if vim.g.blog_conn then
		print("Already connected")
		return true
	end

	print("Trying to establish connection...")
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
		print("Established blog connection")
		return true
	end

	print("Connection error:", vim.inspect(err))
	return false
end

local function establish_connection(ensure_server_started)
	ensure_server_started = ensure_server_started or true
	print("start server?", ensure_server_started)

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
	local _ = nio.run(function()
		-- TODO maybe do something more intelligent here?
		local attempt = 0
		while attempt < 10 do
			print("trying...", attempt)
			attempt = attempt + 1
			nio.sleep(1000)
			if try_connect() then
				return
			end
		end
	end)
end

local autocmd_pattern = blog_path .. "*.{dj,markdown,md}"

autocmd({ "BufRead", "BufNewFile" }, {
	pattern = autocmd_pattern,
	group = blog_group,
	callback = function()
		print("Attached to:", vim.fn.expand("%:p"))
		vim.api.nvim_set_current_dir(blog_path)
		establish_connection(true)
	end,
})

autocmd("CursorMoved", {
	pattern = autocmd_pattern,
	group = blog_group,
	callback = update_position,
})

M = {}

M.start = function()
	start_server()
	establish_connection(false)
end

M.stop = function()
	close_connection()
	stop_server()
end

M.restart = function()
	M.stop()
	M.start()
end

M.reconnect = function()
	close_connection()
	try_connect()
end

M.list_tags = function(cb)
	call({
		id = "ListTags",
	}, cb)
end

M.preview = function() end

return M
