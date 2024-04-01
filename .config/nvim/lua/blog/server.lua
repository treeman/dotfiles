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

-- Server connection management

local blog_group = augroup("blog", { clear = true })

local function close_connection()
	print("Closing existing blog connection")
	vim.fn.chanclose(vim.g.blog_conn)
	vim.g.blog_conn = nil
	-- I guess we could just not create the autocmd, but this is cleaner if we fail to reconnect
	vim.api.nvim_clear_autocmds({ event = "CursorMoved", group = blog_group })
end

local function establish_connection()
	print("Trying to establish connection...")
	local existing = vim.g.blog_conn
	if existing then
		close_connection()
	end

	local status, err = pcall(function()
		vim.g.blog_conn = vim.fn.sockconnect("tcp", "127.0.0.1:8082", {
			on_data = function(_, _, _)
				-- Second argument should be a single-list item,
				-- but since we don't send messages from the blog to Neovim this
				-- should only happen when the connection is closed.
				print("Blog connection closed")
				close_connection()
			end,
		})
	end)

	if not status then
		print("Blog connection error: ", vim.inspect(err))
		return
	end

	print("Established blog connection")
	-- autocmd("CursorMoved", {
	-- 	group = args.group,
	-- 	callback = update_position,
	-- 	buffer = args.buf,
	-- })
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
		establish_connection()
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
		start_server()
		establish_connection()
	end,
})

return M
