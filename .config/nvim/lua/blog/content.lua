local server = require("blog.server")
local path = require("blog/path")
local nio = require("nio")

local M = {}

M.list_tags = function(cb)
	server.call({
		id = "ListTags",
	}, cb)
end

M.list_urls = function(cb)
	server.call({
		id = "ListUrls",
	}, cb)
end

M.list_series = function(cb)
	server.call({
		id = "ListSeries",
	}, cb)
end

M.list_posts = function(subpath, cb)
	nio.run(function()
		local rg = nio.process.run({
			cmd = "rg",
			args = {
				"-NoH",
				"--heading",
				"(title|tags)(: | = )(.+)",
				path.blog_path .. subpath,
			},
		})

		if not rg then
			return
		end

		local output = rg.stdout.read()
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

return M
