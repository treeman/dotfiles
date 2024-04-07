local path = require("blog.path")
local content = require("blog.content")
local nio = require("nio")

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
			'title = "Djot test"',
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
			print("Not a blog file")
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
			print("Not a blog file")
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

M.open_post_in_browser = function(path)
	local rel_path = path.rel_path(path)
	-- How to create url from path?
end

M.open_curr_post_in_browser = function()
	M.open_post_in_browser(vim.fn.expand("%:p"))
end

return M
