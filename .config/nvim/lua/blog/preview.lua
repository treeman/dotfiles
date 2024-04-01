local blog_path = "/home/tree/code/jonashietala/"

local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup

-- Return the path of an open file
local function in_blog(path)
	return path:find(blog_path, 1, true) == 1
end

-- Return the relative path of a file by stripping away the `blog_path`.
local function rel_path(path)
	-- Only strip prefix if found, return original otherwise.
	if path:find(blog_path, 1, true) == 1 then
		return string.sub(path, string.len(blog_path) + 1)
	else
		return path
	end
end

-- local function curr_in_blog()
-- 	return in_blog(vim.fn.expand("%:p"))
-- end

local function path_to_url(path)
	local path = rel_path(path)

	if path:find("drafts/", 1, true) == 1 then
		-- drafts/test.dj -> drafts/test
	elseif path:find("posts/", 1, true) == 1 then
		-- posts/2024-01-01-some_title.dj -> blog/2024/01/01/some_title
	end
end

local function path_to_localhost(path)
	return "localhost:8080/" .. path_to_url(path)
end

local function url_to_path(url) end

-- This fun little thing tries to connect to my blogging
-- watch server and sends it document positions on move.
local function update_position(args)
	local msg = vim.fn.json_encode({
		id = "CursorMoved",
		-- context = vim.fn.getline("."),
		linenum = vim.fn.line("."),
		linecount = vim.fn.line("$"),
		column = vim.fn.col("."),
		path = vim.fn.expand("%:p"),
	})
	-- print(vim.inspect(msg))

	local conn = vim.b[args.buf].blog_conn
	vim.fn.chansend(conn, msg)
	-- Watcher tries to read lines so we need to terminate the message with a newline
	vim.fn.chansend(conn, "\n")
end

local function clear_connection(args)
	print("Closing existing blog connection")
	vim.fn.chanclose(vim.b[args.buf].blog_conn)
	vim.b[args.buf].blog_conn = nil
	-- I guess we could just not create the autocmd, but this is cleaner if we fail to reconnect
	vim.api.nvim_clear_autocmds({ event = "CursorMoved", buffer = args.buffer, group = args.group })
end

local function establish_connection(args)
	print("Trying to establish connection...")
	local existing = vim.b[args.buf].blog_conn
	if existing then
		clear_connection(args)
	end

	local status, err = pcall(function()
		vim.b[args.buf].blog_conn = vim.fn.sockconnect("tcp", "127.0.0.1:8082", {
			on_data = function(_, _, _)
				-- Second argument should be a single-list item,
				-- but since we don't send messages from the blog to Neovim this
				-- should only happen when the connection is closed.
				print("Blog connection closed")
				clear_connection(args)
			end,
		})
	end)

	if not status then
		print("Blog connection error: ", vim.inspect(err))
		return
	end

	print("Established blog connection")
	autocmd("CursorMoved", {
		group = args.group,
		callback = update_position,
		buffer = args.buf,
	})
end

local function attach(args)
	local curr_path = vim.fn.expand("%:p")
	if curr_path:find(blog_path, 1, true) ~= 1 then
		return
	end

	establish_connection(args)
end

local group = augroup("blog", { clear = true })
autocmd({ "BufRead", "BufNewFile" }, {
	pattern = "*.dj,*.markdown",
	group = group,
	callback = attach,
})

M = {}

M.preview_curr_buf = function()
	local curr_path = vim.fn.expand("%:p")
	if not in_blog(curr_path) then
		return
	end

	local url = path_to_localhost(curr_path)
	print(url)
end

return M
