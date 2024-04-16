local path = require("blog.path")
local content = require("blog.content")
local nio = require("nio")
local log = require("plenary.log").new({
	plugin = "blog",
	level = "error",
})

local M = {}

-- Create a slug from a title.
M.slugify = function(title)
	title = title:lower()
	title = title:gsub("[^ a-zA-Z0-9_-]+", "")
	title = title:gsub("[ _]+", "_")
	title = title:gsub("^[ _-]+", "")
	title = title:gsub("[ _-]+$", "")
	return title
end

M.new_draft = function()
	nio.run(function()
		local title = nio.ui.input({ prompt = "Draft title: " })
		local file_path = path.blog_path .. "drafts/" .. M.slugify(title) .. ".dj"
		vim.cmd(":e " .. file_path)
		local template = {
			"---toml",
			'title = "' .. title .. '"',
			'tags = ["Some tag"]',
			"---",
		}
		local buf = vim.api.nvim_get_current_buf()
		vim.api.nvim_buf_set_lines(buf, 0, 0, true, template)
	end)
end

local function _rename(from, to)
	vim.cmd(":!mv " .. from .. " " .. to)
	vim.cmd(":e " .. to)
end

M.promote_draft = function(draft_path)
	nio.run(function()
		if not path.in_blog(draft_path) then
			log.error("Not a blog file:", draft_path)
			return
		end

		local title = content.extract_title(draft_path)
		local post_path = path.blog_path .. "posts/" .. os.date("%Y-%m-%d") .. "-" .. M.slugify(title) .. ".dj"

		nio.scheduler()
		_rename(draft_path, post_path)
	end)
end

M.promote_curr_draft = function()
	M.promote_draft(vim.fn.expand("%:p"))
end

M.demote_post = function(post_path)
	nio.run(function()
		if not path.in_blog(post_path) then
			log.error("Not a blog file:", post_path)
			return
		end

		local title = content.extract_title(post_path)
		local draft_path = path.blog_path .. "drafts/" .. M.slugify(title) .. ".dj"

		nio.scheduler()
		_rename(post_path, draft_path)
	end)
end

M.demote_curr_post = function()
	M.demote_post(vim.fn.expand("%:p"))
end

M.path_to_url = function(file_path)
	local host = "localhost:8080/"
	local rel_path = path.rel_path(file_path)
	if path.is_draft(rel_path) then
		local slug = rel_path:match("drafts/(.+)%.")
		return host .. "drafts/" .. slug
	elseif path.is_post(rel_path) then
		local date, slug = rel_path:match("posts/(%d%d%d%d%-%d%d%-%d%d)-(.+)%.")
		return host .. "blog/" .. date:gsub("-", "/") .. "/" .. slug
	else
		return nil
	end
end

M.open_post_in_browser = function(file_path)
	local url = M.path_to_url(file_path)
	if not url then
		log.error("Could not convert to url:", file_path)
		return
	end

	vim.cmd("!firefox " .. url)
end

M.open_curr_post_in_browser = function()
	M.open_post_in_browser(vim.fn.expand("%:p"))
end

return M
