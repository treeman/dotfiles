P = function(v)
	print(vim.inspect(v))
	return v
end

local M = {}

M.create_cmd = function(command, f, opts)
	opts = opts or {}
	vim.api.nvim_create_user_command(command, f, opts)
end

M.has_normal_keyboard = function()
	return os.getenv("NORMAL_KEYBOARD")
end

M.list_buffers = function()
	return vim.tbl_filter(function(buf)
		return vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_buf_get_option(buf, "buflisted")
	end, vim.api.nvim_list_bufs())
end

return M
