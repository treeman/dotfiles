-- :BlogStart
-- :BlogStop
-- :BlogRestart
-- :BlogPreview
--
-- Should start `blog watch` in terminal when opening a dj or md file in our blog
-- Should show connection + server status in statusbar
-- Should cwd to blog_path when loading a file

local blog_path = "/home/tree/code/jonashietala/"

local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup

-- Event handling

-- This fun little thing tries to connect to my blogging
-- watch server and sends it document positions on move.
local function update_position(args)
	local conn = vim.g.blog_conn

	if not conn then
		return
	end

	local msg = vim.fn.json_encode({
		id = "CursorMoved",
		-- context = vim.fn.getline("."),
		linenum = vim.fn.line("."),
		linecount = vim.fn.line("$"),
		column = vim.fn.col("."),
		path = vim.fn.expand("%:p"),
	})
	-- print(vim.inspect(msg))

	vim.fn.chansend(conn, msg)
	-- Watcher tries to read lines so we need to terminate the message with a newline
	vim.fn.chansend(conn, "\n")
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

local function restart_server()
	stop_server()
	start_server()
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
	-- I guess we could just not create the autocmd, but this is cleaner if we fail to reconnect
	vim.api.nvim_clear_autocmds({ event = "CursorMoved", group = blog_group })
end

local function try_connect()
	if vim.g.blog_conn then
		print("Already connected")
		return true
	end

	print("Trying to establish connection...")
	local status, err = pcall(function()
		vim.g.blog_conn = vim.fn.sockconnect("tcp", "127.0.0.1:8082", {
			on_data = function(a, b, c)
				-- Second argument should be a single-list item,
				-- but since we don't send messages from the blog to Neovim this
				-- should only happen when the connection is closed.
				print("Blog connection closed")
				print(vim.inspect(a))
				print(vim.inspect(b))
				print(vim.inspect(c))
				close_connection()
			end,
		})
	end)

	if status then
		print("Established blog connection")

		-- autocmd("CursorMoved", {
		-- 	pattern = blog_path .. "*.{dj,markdown,md}",
		-- 	group = blog_group,
		-- 	callback = update_position,
		-- })

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
	local nio = require("nio")
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

M = {}

M.start = start_server
M.stop = stop_server
M.restart = restart_server

M.preview = function() end

autocmd({ "BufRead", "BufNewFile" }, {
	pattern = blog_path .. "*.{dj,markdown,md}",
	group = blog_group,
	callback = function()
		print("Attached to:", vim.fn.expand("%:p"))
		vim.api.nvim_set_current_dir(blog_path)
		-- TODO
		-- 1. try to establish connection
		-- 2. if fails, try to start_server, and try to connect again
		--	  unless we have done :BlogStop, which should block it via a global flag...??
		-- start_server()
		establish_connection(true)
	end,
})

return M
