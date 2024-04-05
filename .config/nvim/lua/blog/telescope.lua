local action_state = require("telescope.actions.state")
local actions = require("telescope.actions")
local config = require("telescope.config").values
local finders = require("telescope.finders")
local pickers = require("telescope.pickers")
local previewers = require("telescope.previewers")
local server = require("blog/server")

local M = {}

M.show_tags = function(opts)
	print("show tags")
	server.list_tags(function(_, res)
		if res then
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
					sorter = config.generic_sorter(opts),
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
		end
	end)
end

M.show_tags()

-- TODO these should just use fd and rg so we don't have to wait
-- Browse posts
-- Browse drafts

return M
