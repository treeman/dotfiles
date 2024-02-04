-- This fun little thing tries to connect to my blogging
-- watch server and sends it document positions on move.
local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup

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

	local conn = vim.b[args.buf].myblog_conn
	vim.fn.chansend(conn, msg)
	-- Watcher tries to read lines so we need to terminate the message with a newline
	vim.fn.chansend(conn, "\n")
end

local function clear_connection(args)
	print("Closing existing blog connection")
	vim.fn.chanclose(vim.b[args.buf].myblog_conn)
	vim.b[args.buf].myblog_conn = nil
	-- I guess we could just not create the autocmd, but this is cleaner if we fail to reconnect
	vim.api.nvim_clear_autocmds({ event = "CursorMoved", buffer = args.buffer, group = args.group })
end

local function establish_connection(args)
	print("Trying to establish connection...")
	local existing = vim.b[args.buf].myblog_conn
	if existing then
		clear_connection(args)
	end

	local status, err = pcall(function()
		vim.b[args.buf].myblog_conn = vim.fn.sockconnect("tcp", "127.0.0.1:8082", {
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
	local blog_path = "/home/tree/code/jonashietala/"
	if curr_path:find(blog_path, 1, true) ~= 1 then
		return
	end

	establish_connection(args)
end

local group = augroup("myblog", { clear = true })
autocmd({ "BufRead", "BufNewFile" }, {
	pattern = "*.dj,*.markdown",
	group = group,
	callback = attach,
})
