local action_state = require("telescope.actions.state")
local actions = require("telescope.actions")
local conf = require("telescope.config").values
local finders = require("telescope.finders")
local pickers = require("telescope.pickers")
local previewers = require("telescope.previewers")
local sorters = require("telescope.sorters")
local content = require("blog.content")

local tags_sorter = function(opts)
	opts = opts or {}
	local fzy = opts.fzy_mod or require("telescope.algos.fzy")

	return sorters.Sorter:new({
		discard = true,

		scoring_function = function(_, prompt, entry)
			-- First filter using fzy.
			if not fzy.has_match(prompt, entry.name) then
				return -1
			end

			-- Then order tags by post count.
			if entry.post_count == 0 then
				return -1
			else
				return 1 / entry.post_count
			end
		end,

		highlighter = function(_, prompt, display)
			return fzy.positions(prompt, display)
		end,
	})
end

local posts_sorter = function(opts)
	opts = opts or {}
	local fzy_sorter = sorters.get_fzy_sorter(opts)

	return sorters.Sorter:new({
		discard = true,

		scoring_function = function(_, prompt, entry)
			-- We could separate tags and series and calculate them separately...

			-- fzy generates a score 0..1, and < 0 for filtered entries.
			local entry_string = ""
			if entry.series then
				entry_string = entry.series .. ":"
			end
			entry_string = entry_string .. entry.tags .. ":" .. entry.title

			local base_score = fzy_sorter:scoring_function(prompt, entry_string)
			if base_score < 0 then
				return base_score
			end

			-- Date of my first blog post as a number.
			local beginning_of_time = 20090621
			-- Today's date as a number.
			local today = os.date("%Y%m%d")
			-- Remove `-` from entry date, so it's comparable.
			local entry_time = string.gsub(entry.date, "-", "")
			-- Place the number on a 0..1 scale, where 1 is today (`1 -` reverses, otherwise 1
			-- would be the beginning of time).
			local date_score = 1 - (entry_time - beginning_of_time) / (today - beginning_of_time)

			-- Date sorting is only worth 1/10 of the fuzzy score.
			-- Why? I dunno, it's arbitrary from how it feels.
			return base_score + date_score / 10
		end,

		highlighter = fzy_sorter.highlighter,
	})
end

local M = {}

M.find_tags = function(opts)
	content.list_tags(function(reply)
		pickers
			.new(opts, {
				finder = finders.new_table({
					results = reply.tags,
					entry_maker = function(entry)
						return {
							display = entry.name .. " (" .. tostring(#entry.posts) .. ")",
							ordinal = { name = entry.name, post_count = #entry.posts },
							value = entry,
						}
					end,
				}),
				sorter = tags_sorter(opts),
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

local function _find_post(subpath)
	content.list_posts(subpath, function(posts)
		pickers
			.new(opts, {
				finder = finders.new_table({
					results = posts,
					entry_maker = function(entry)
						local title = entry.title
						if entry.date then
							title = title .. " (" .. entry.date .. ")"
						end

						local ordinal = {
							title = entry.title,
							tags = entry.tags,
							series = entry.series,
							date = entry.date,
						}

						return {
							display = title,
							ordinal = ordinal,
							value = entry,
						}
					end,
				}),
				sorter = posts_sorter(opts),
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

M.find_post = function()
	return _find_post("posts/")
end

M.find_draft = function()
	return _find_post("drafts/")
end

return M
