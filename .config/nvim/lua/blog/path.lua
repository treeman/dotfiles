M = {}

-- Not defined as `M` because lib loading sometimes causes it to be nil...
local blog_path = vim.fn.expand("$HOME/code/jonashietala/")

M.in_blog = function(path)
	return path:find(blog_path, 1, true) == 1
end

M.is_post = function(path)
	return path:find("posts/", 1, true) == 1
end

M.is_draft = function(path)
	return path:find("drafts/", 1, true) == 1
end

-- Return the relative path of a file by stripping away the `blog_path`.
M.rel_path = function(path)
	-- Only strip prefix if found, return original otherwise.
	if path:find(blog_path, 1, true) == 1 then
		return string.sub(path, string.len(blog_path) + 1)
	else
		return path
	end
end

M.path_to_url = function(file_path)
	local host = "localhost:8080/"
	local rel_path = M.rel_path(file_path)
	if M.is_draft(rel_path) then
		local slug = rel_path:match("drafts/(.+)%.")
		return host .. "drafts/" .. slug
	elseif M.is_post(rel_path) then
		local date, slug = rel_path:match("posts/(%d%d%d%d%-%d%d%-%d%d)-(.+)%.")
		return host .. "blog/" .. date:gsub("-", "/") .. "/" .. slug
	else
		return nil
	end
end

-- Create a slug from a title.
M.slugify = function(title)
	title = title:lower()
	title = title:gsub("[^ a-zA-Z0-9_-]+", "")
	title = title:gsub("[ _]+", "_")
	title = title:gsub("^[ _-]+", "")
	title = title:gsub("[ _-]+$", "")
	return title
end

M.blog_path = blog_path

return M
