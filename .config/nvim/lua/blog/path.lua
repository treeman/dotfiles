local blog_path = vim.fn.expand("$HOME/code/jonashietala/")

local function in_blog(path)
	return path:find(blog_path, 1, true) == 1
end

local function is_post(path)
	return path:find("posts/", 1, true) == 1
end

local function is_draft(path)
	return path:find("drafts/", 1, true) == 1
end

-- Return the relative path of a file by stripping away the `blog_path`.
local function rel_path(path)
	-- Only strip prefix if found, return original otherwise.
	if path:find(blog_path, 1, true) == 1 then
		return string.sub(path, string.len(blog_path) + 1)
	else
		return path
	end
end

local function path_to_url(file_path)
	local host = "localhost:8080/"
	local rel_file_path = rel_path(file_path)
	if is_draft(rel_file_path) then
		local slug = rel_file_path:match("drafts/(.+)%.")
		return host .. "drafts/" .. slug
	elseif is_post(rel_file_path) then
		local date, slug = rel_file_path:match("posts/(%d%d%d%d%-%d%d%-%d%d)-(.+)%.")
		return host .. "blog/" .. date:gsub("-", "/") .. "/" .. slug
	else
		return nil
	end
end

-- Create a slug from a title.
local function slugify(title)
	title = title:lower()
	title = title:gsub("[^ a-zA-Z0-9_-]+", "")
	title = title:gsub("[ _]+", "_")
	title = title:gsub("^[ _-]+", "")
	title = title:gsub("[ _-]+$", "")
	return title
end

-- Define `M` here because lib loading sometimes makes these nil...?
M = {}

M.blog_path = blog_path
M.rel_path = rel_path
M.in_blog = in_blog
M.is_post = is_post
M.is_draft = is_draft
M.path_to_url = path_to_url
M.slugify = slugify

return M
