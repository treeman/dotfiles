M = {}

local function extract_element_url(element)
  if element.url then
    return element.url
  end

  if element.link_ref then
    return element.link_ref.url
  end
end

local function get_element_url(element)
  local url = extract_element_url(element)
  if url and url:match("^/") then
    return "localhost:8080" .. url
  else
    return url
  end
end

local function static_open(old_gx)
  -- Open git short links.
  local plugin_name = vim.fn.expand("<cWORD>"):match("[\"']([%a_%.%-]+/[%a_%.%-]+)[\"']")
  if plugin_name then
    vim.ui.open("https://github.com/" .. plugin_name)
    return
  end

  if old_gx then
    old_gx()
  end
end

M.gx_extended = function(old_gx)
  return function()
    if require("blog.server").is_buf_connected() then
      require("blog.content").cursor_info(function(reply)
        if reply and reply.element then
          local url = get_element_url(reply.element)
          if url then
            vim.ui.open(url)
            return
          end
        end
        static_open(old_gx)
      end)
    else
      static_open(old_gx)
    end
  end
end

return M
