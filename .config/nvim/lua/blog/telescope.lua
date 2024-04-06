local action_state = require("telescope.actions.state")
local actions = require("telescope.actions")
local conf = require("telescope.config").values
local finders = require("telescope.finders")
local pickers = require("telescope.pickers")
local previewers = require("telescope.previewers")
local server = require("blog/server")
local path = require("blog/path")
local nio = require("nio")

local M = {}

M.find_tags = function(opts)
	server.list_tags(function(_, res)
		if not res then
			return
		end

		pickers
			.new(opts, {
				finder = finders.new_table({
					results = res.tags,
					entry_maker = function(entry)
						return {
							display = entry.name .. " (" .. tostring(#entry.posts) .. ")",
							ordinal = entry.name,
							value = entry,
						}
					end,
				}),
				-- TODO sort tags by count?
				sorter = conf.generic_sorter(opts),
				previewer = previewers.new_buffer_previewer({
					title = "Tag detaiLs",
					define_preview = function(self, entry)
						local lines = {}
						for _, post in ipairs(entry.value.posts) do
							table.insert(lines, post.title)
							table.insert(lines, post.url)
							table.insert(lines, "")
						end
						vim.api.nvim_buf_set_lines(self.state.bufnr, 0, 0, true, lines)
					end,
				}),
				attach_mappings = function(prompt_bufnr)
					actions.select_default:replace(function()
						local selection = action_state.get_selected_entry()
						actions.close(prompt_bufnr)
						-- TODO multiselect to insert all...?
						vim.cmd(':normal i "' .. selection.value.name .. '"')
					end)
					return true
				end,
			})
			:find()
	end)
end

local function _find_posts(subpath)
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
				table.insert(posts, post)
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
					P(date)
					if date then
						post["date"] = date
					end
				end
			end
		end

		pickers
			.new(opts, {
				finder = finders.new_table({
					results = posts,
					entry_maker = function(entry)
						local date = ""
						if entry.date then
							date = " (" .. entry.date .. ")"
						end

						return {
							display = entry.title .. date,
							ordinal = entry.title .. entry.tags,
							value = entry,
						}
					end,
				}),
				sorter = conf.file_sorter(opts),
				previewer = previewers.new_buffer_previewer({
					title = "Post Preview",
					define_preview = function(self, entry)
						conf.buffer_previewer_maker(entry.value.path, self.state.bufnr, {
							bufname = self.state.bufname,
							winid = self.state.winid,
							preview = opts.preview,
							file_encoding = opts.file_encoding,
						})
					end,
				}),
				attach_mappings = function(prompt_bufnr)
					actions.select_default:replace(function()
						local selection = action_state.get_selected_entry()
						actions.close(prompt_bufnr)
						vim.cmd(":e " .. selection.value.path)
					end)
					return true
				end,
			})
			:find()
	end)
end

M.find_posts = function()
	return _find_posts("posts/")
end

M.find_drafts = function()
	return _find_posts("drafts/")
end

-- M.find_drafts()
-- M.find_posts()

return M
