local action_state = require("telescope.actions.state")
local actions = require("telescope.actions")
local conf = require("telescope.config").values
local finders = require("telescope.finders")
local pickers = require("telescope.pickers")
local previewers = require("telescope.previewers")
local content = require("blog.content")

local M = {}

M.find_tags = function(opts)
	content.list_tags(function(reply)
		pickers.new(opts, {
			finder = finders.new_table({
				results = reply.tags,
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
		}):find()
	end)
end

local function _find_post(subpath)
	content.list_posts(subpath, function(posts)
		pickers.new(opts, {
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
		}):find()
	end)
end

M.find_post = function()
	return _find_post("posts/")
end

M.find_draft = function()
	return _find_post("drafts/")
end

return M
