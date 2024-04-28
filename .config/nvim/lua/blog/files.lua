local path = require("blog.path")
local content = require("blog.content")
local utils = require("util.utils")
local nio = require("nio")
local log = require("plenary.log").new({
	plugin = "blog",
	level = "error",
})

local M = {}

local function create_file(title_prompt, folder, template_fun)
	nio.run(function()
		local title = nio.ui.input({ prompt = title_prompt })
		local template = template_fun(title)
		local file_path = path.blog_path .. folder .. path.slugify(title) .. ".dj"
		vim.cmd(":e " .. file_path)
		vim.api.nvim_buf_set_lines(0, 0, 0, true, template)
	end)
end

M.new_draft = function()
	create_file("Draft title: ", "drafts/", function(title)
		return {
			"---toml",
			'title = "' .. title .. '"',
			'tags = ["Some tag"]',
			"---",
		}
	end)
end

M.new_series = function()
	create_file("Series title: ", "series/", function(title)
		return {
			"---toml",
			'title = "' .. title .. '"',
			"completed = false",
			'img = "/images/trident/nice_wires2.jpg"',
			"homepage = true",
			"---",
		}
	end)
end

M.new_static = function()
	create_file("Static title: ", "static/", function(title)
		return {
			"---toml",
			'title = "' .. title .. '"',
			"---",
		}
	end)
end

M.new_project = function()
	utils.list_files(path.blog_path .. "projects/", function(files)
		local lowest_ordinal = 99999
		for _, file in ipairs(files) do
			local ord = tonumber(string.match(file, "projects/(%d+)"))
			if ord and ord < lowest_ordinal then
				lowest_ordinal = ord
			end
		end

		local title = nio.ui.input({ prompt = "Project title: " })
		local template = {
			"---toml",
			'title = "' .. title .. '"',
			"year = " .. os.date("%Y"),
			'link = ""',
			"homepage = true",
			"---",
		}
		local file_path = path.blog_path .. "projects/" .. lowest_ordinal - 1 .. "_" .. path.slugify(title) .. ".dj"
		vim.cmd(":e " .. file_path)
		vim.api.nvim_buf_set_lines(0, 0, 0, true, template)
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
		local post_path = path.blog_path .. "posts/" .. os.date("%Y-%m-%d") .. "-" .. path.slugify(title) .. ".dj"

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
		local draft_path = path.blog_path .. "drafts/" .. path.slugify(title) .. ".dj"

		nio.scheduler()
		_rename(post_path, draft_path)
	end)
end

M.demote_curr_post = function()
	M.demote_post(vim.fn.expand("%:p"))
end

M.open_post_in_browser = function(file_path)
	local url = path.path_to_url(file_path)
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
