local server = require("blog.server")
local path = require("blog.path")
local nio = require("nio")

local M = {}

M.list_link_defs = function(cb)
	server.call({
		id = "ListLinkDefs",
		path = vim.fn.expand("%:p"),
	}, cb)
end

M.list_headings = function(path, cb)
	server.call({
		id = "ListHeadings",
		path = path,
	}, cb)
end

M.list_headings_in_curr = function(cb)
	M.list_headings(vim.fn.expand("%:p"), cb)
end

M.list_urls = function(cb)
	server.call({
		id = "ListUrls",
	}, cb)
end

M.list_tags = function(cb)
	server.call({
		id = "ListTags",
	}, cb)
end

M.list_series = function(cb)
	server.call({
		id = "ListSeries",
	}, cb)
end

M.run_cmd = function(args)
	local rg = nio.process.run(args)

	if not rg then
		return nil
	end

	return rg.stdout.read()
end

M.extract_title = function(path)
	local title = M.run_cmd({
		cmd = "rg",
		args = {
			"-NoH",
			"^title = (.+)",
			path,
		},
	})

	if not title then
		return nil
	end

	return title:match('title = "(.+)"')
end

M.list_posts = function(subpath, cb)
	nio.run(function()
		local output = M.run_cmd({
			cmd = "rg",
			args = {
				"-NoH",
				"--heading",
				"(title|tags)(: | = )(.+)",
				path.blog_path .. subpath,
			},
		})

		if not output then
			return
		end

		nio.scheduler()
		local lines = vim.fn.split(output, "\n")
		local posts = {}

		local line_count = #lines
		local post = {}
		for i = 1, line_count do
			local line = lines[i]

			if line == "" then
				if post.title then
					table.insert(posts, post)
				end
				post = {}
			else
				local title = string.match(line, 'title%s?[:=]%s+"(.+)"')
				local tags = string.match(line, "tags%s?[:=]%s+(.+)")
				if title then
					post["title"] = title
				elseif tags then
					post["tags"] = tags
				else
					post["path"] = line
					local date = string.match(line, "posts/(%d%d%d%d%-%d%d%-%d%d)%-")
					if date then
						post["date"] = date
					end
				end
			end
		end

		cb(posts)
	end)
end

M.list_images = function(cb)
	nio.run(function()
		local output = M.run_cmd({
			cmd = "fd",
			args = {
				"-t",
				"f",
				"\\.",
				path.blog_path .. "images/",
			},
		})

		if not output then
			return
		end

		nio.scheduler()
		local images = vim.fn.split(output, "\n")
		cb(images)
	end)
end

return M
