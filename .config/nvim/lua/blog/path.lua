M = {}

M.blog_path = "/home/tree/code/jonashietala/"

-- Return the relative path of a file by stripping away the `blog_path`.
M.rel_path = function(path)
	-- Only strip prefix if found, return original otherwise.
	if path:find(M.blog_path, 1, true) == 1 then
		return string.sub(path, string.len(M.blog_path) + 1)
	else
		return path
	end
end

return M