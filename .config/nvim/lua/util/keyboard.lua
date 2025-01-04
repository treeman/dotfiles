local M = {}

function M.has_custom_keyboard_layout()
  return os.getenv("CUSTOM_KEYBOARD")
end

return M
