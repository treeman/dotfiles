local action_state = require("telescope.actions.state")
local actions = require("telescope.actions")
local conf = require("telescope.config").values
local finders = require("telescope.finders")
local pickers = require("telescope.pickers")
local previewers = require("telescope.previewers")
local sorters = require("telescope.sorters")
local content = require("blog.content")

local M = {}

local function tags_sorter(opts)
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

local function split_prompt(prompt)
	local tags = {}
	local series = {}
	local title = {}
	local path = {}
	for word in prompt:gmatch("([^%s]+)") do
		local fst = word:sub(1, 1)
		if fst == "@" then
			table.insert(tags, word:sub(2))
		elseif fst == "#" then
			table.insert(series, word:sub(2))
		elseif fst == "/" then
			table.insert(path, word)
		else
			table.insert(title, word)
		end
	end

	return {
		tags = tags,
		series = series,
		path = path,
		title = vim.fn.join(title, " "),
	}
end

-- Score a prompt element against an entry element using `sorter`.
--
-- Prompt elements should be a list of words from the prompt.
-- For example a prompt with tags like `@tag1 @tag2`
-- can produce the `{ "tag1", "tag2" }` `prompt_elements`.
-- Every prompted element needs a match, otherwise the entry will
-- get filtered.
--
-- The `entry_element` could either be a string (such a series id)
-- or a list of strings (multiple tags).
--
-- This function will clamp the score to 0..1, and return -1
-- if it should remove the entry (we prompted for it, but got no match).
-- In the case of multiple prompt elements, each individual element
-- is worth 1 / count prompt_elements.
local function score_element(prompt_elements, entry_element, sorter)
	-- We didn't prompt for this type, ignore it.
	if next(prompt_elements) == nil then
		return 0
	end

	-- We prompted for this type, but entry didn't have it, so remove the entry.
	-- For example if we prompt for a series, this removes all posts
	-- without a series.
	if not entry_element then
		return -1
	end

	-- Convert multiple entry values to a string like `tag1:tag2`.
	local entry
	if type(entry_element) == "string" then
		entry = entry_element
	elseif type(entry_element) == "table" then
		entry = vim.fn.join(entry_element, ":")
	end

	local total = 0
	for _, prompt_element in ipairs(prompt_elements) do
		local score = sorter:scoring_function(prompt_element, entry)
		-- Require a match for every element.
		if score < 0 then
			return -1
		end
		total = total + score
	end

	-- Clamp score to max 1.
	return total / #prompt_elements
end

local function posts_sorter(opts)
	opts = opts or {}
	local fzy_sorter = sorters.get_fzy_sorter(opts)

	return sorters.Sorter:new({
		discard = true,

		scoring_function = function(_, prompt, entry)
			prompt = split_prompt(prompt)

			-- Score and filter against series, tags, and title separately.
			-- If any element returns a 0, it means nothing matched but we shouldn't filter.
			-- If it returns < 0, it means it did not match and should remove the entry.
			local series_score = score_element(prompt.series, entry.series, fzy_sorter)
			if series_score < 0 then
				return -1
			end

			local tags_score = score_element(prompt.tags, entry.tags, fzy_sorter)
			if tags_score < 0 then
				return -1
			end

			local path_score = score_element(prompt.path, entry.path, fzy_sorter)
			if path_score < 0 then
				return -1
			end

			local title_score = fzy_sorter:scoring_function(prompt.title, entry.title)
			if title_score < 0 then
				return -1
			end

			-- Date of my first blog post as a number.
			local beginning_of_time = 20090621
			-- Today's date as a number.
			local today = os.date("%Y%m%d")
			-- Remove `-` from entry date, so it's like a number.
			local entry_date = string.gsub(entry.date, "-", "")
			-- Place the number on a 0..1 scale, where 1 is today (`1 -` reverses, otherwise 1
			-- would be the beginning of time).
			local date_score = 1 - (entry_date - beginning_of_time) / (today - beginning_of_time)

			-- Date sorting is only worth 1/10 of the fuzzy scores.
			-- Why? I dunno, it felt like 1 was too much and 1/10 felt good.
			return series_score + tags_score + title_score + date_score / 10
		end,
	})
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

						return {
							display = title,
							ordinal = entry,
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

local function post_sorter(opts)
	opts = opts or {}
	local fzy_sorter = sorters.get_fzy_sorter(opts)

	return sorters.Sorter:new({
		discard = true,

		scoring_function = function(_, prompt, entry)
			prompt = split_prompt(prompt)

			local series_score = score_element(prompt.series, entry.series, fzy_sorter)
			if series_score < 0 then
				return -1
			end

			local tags_score = score_element(prompt.tags, entry.tags, fzy_sorter)
			if tags_score < 0 then
				return -1
			end

			local title_score = fzy_sorter:scoring_function(prompt.title, entry.title)
			if title_score < 0 then
				return -1
			end

			return series_score + tags_score + title_score
		end,

		-- highlighter = fzy_sorter.highlighter,
	})
end

local function tmp()
	pickers
		.new(opts, {
			finder = finders.new_table({
				results = {
					-- `tags`
					{ title = "One", tags = { "Tag1", "Tag2" }, path = "posts/2024-01-01-one.dj" },
					{ title = "Two", tags = { "Tag2" }, path = "posts/2024-02-02-two.dj" },
				},
				entry_maker = function(entry)
					return {
						display = entry.title,
						-- `ordinal` is now a list.
						ordinal = entry,
					}
				end,
			}),
			sorter = post_sorter(opts),
		})
		:find()
end

-- tmp()

return M
