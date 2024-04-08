M = {}

-- Not defined as `M` because lib loading sometimes causes it to be nil...
local blog_path = "/home/tree/code/jonashietala/"

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

M.blog_path = blog_path

return M
