local M = {}

M.has_normal_keyboard = function()
  return os.getenv("NORMAL_KEYBOARD")
end

return M

