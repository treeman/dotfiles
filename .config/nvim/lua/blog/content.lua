local server = require("blog.server")
local path = require("blog.path")
local nio = require("nio")
local util = require("util")

local M = {}

M.list_tags = function(cb)
	server.call({
		id = "ListTags",
	}, cb)
end

M.run_cmd = function(args)
	local proc = nio.process.run(args)

	if not proc then
		return nil
	end

	return proc.stdout.read()
end

M.extract_title = function(file_path)
	local title = M.run_cmd({
		cmd = "rg",
		args = {
			"-NoH",
			"^title = (.+)",
			file_path,
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
				"-NoHU",
				"--heading",
				"\\A\\---\\w*\\n(.+\\n)+^---",
				path.blog_path .. subpath,
			},
		})

		if not output then
			return
		end

		nio.scheduler()
		local lines = vim.fn.split(output, "\n")
		local posts = {}

		local post = {}
		for _, line in ipairs(lines) do
			-- When a newline is encountered save the post and prepare for the next entry.
			if line == "" then
				if post.title then
					table.insert(posts, post)
				end
				post = {}
				-- Skip `---` markers.
			elseif not string.match(line, "%-%-%-%w*") then
				-- Try to extract all key value definitions and store them.
				local key, value = string.match(line, "(%w+)%s*[:=]%s*(.+)")
				if key then
					-- Strip surrounding quotes.
					-- Do this here because there's no non-greedy specifier that could be used
					-- in the key/value regex above.
					local stripped = string.match(value, '^"(.+)"$')
					if stripped then
						value = stripped
					end
					post[key] = value
				else
					-- If no key value pair is found, then we should be at the beginning with the file path.
					post["path"] = line
					-- Only posts have a date in the path, not drafts.
					local date = string.match(line, "posts/(%d%d%d%d%-%d%d%-%d%d)%-")
					if date then
						post["date"] = date
					else
						post["date"] = os.date("%Y-%m-%d", util.file_modified(line))
					end
				end
			end
		end
		if post.title then
			table.insert(posts, post)
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
