M = {}

M.gx_extended = function(fallback)
  return function()
    if require("blog.server").is_buf_connected() then
      -- require("blog.content").cursor_info(function(reply)
      --   P(reply)
      --   if reply and reply.element and reply.element.link_ref and reply.element.link_ref.url then
      --     vim.ui.open("localhost:8080" .. reply.element.link_ref.url)
      --   else
      --     if fallback then
      --       fallback()
      --     end
      --   end
      -- end)
      -- return
      -- TODO rely on backend instead to give us the url.
      -- Will allow us to use `gx` on a short link for example.
      local rel_path = vim.fn.expand("<cWORD>"):match("%]%(([^%)]+)%)$")
        or vim.fn.expand("<cWORD>"):match("^(/[^%s]+)$")

      if rel_path then
        vim.ui.open("localhost:8080" .. rel_path)
        return
      end
    end

    local plugin_name = vim.fn.expand("<cWORD>"):match("[\"']([%a_%.%-]+/[%a_%.%-]+)[\"']")
    if plugin_name then
      vim.ui.open("https://github.com/" .. plugin_name)
      return
    end

    if fallback then
      fallback()
    end
  end
end

return M
