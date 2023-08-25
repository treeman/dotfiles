local action_state = require("telescope.actions.state")
local actions = require("telescope.actions")

local Path = require("plenary.path")

local m = {}

-- https://github.com/nvim-telescope/telescope.nvim/issues/621
m.open_buffer = function()
	require("telescope.builtin").buffers({
		attach_mappings = function(prompt_bufnr, map)
			local delete_buf = function()
				local current_picker = action_state.get_current_picker(prompt_bufnr)
				local multi_selections = current_picker:get_multi_selection()

				if next(multi_selections) == nil then
					local selection = action_state.get_selected_entry()
					actions.close(prompt_bufnr)
					vim.api.nvim_buf_delete(selection.bufnr, { force = true })
				else
					actions.close(prompt_bufnr)
					for _, selection in ipairs(multi_selections) do
						vim.api.nvim_buf_delete(selection.bufnr, { force = true })
					end
				end
			end

			map("i", "<C-u>", delete_buf)
			return true
		end,
	})
end

m.open_norg = function(base_folder)
	local folder = vim.fn.expand("~/norg/") .. base_folder .. "/"

	require("telescope.builtin").find_files({
		attach_mappings = function(prompt_bufnr, map)
			local create_file = function()
				-- It ain't pretty... But maybe it's good enough...? T.T
				local current_picker = action_state.get_current_picker(prompt_bufnr)
				local input = folder .. current_picker:_get_prompt() .. ".norg"

				local file = Path:new(input)
				if file:exists() then
					return
				end
				file:touch({ parents = true })

				actions.close(prompt_bufnr)
				vim.cmd("e " .. file .. "| w")
			end

			map("i", "<C-e>", create_file)
			return true
		end,
		cwd = folder,
	})
end

return m
