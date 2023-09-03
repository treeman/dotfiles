local M = {}

M.create_cmd = function(command, f, opts)
	opts = opts or {}
	vim.api.nvim_create_user_command(command, f, opts)
end

M.has_normal_keyboard = function()
	return os.getenv("NORMAL_KEYBOARD")
end

return M
