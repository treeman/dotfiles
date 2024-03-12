local M = {}

-- Find files using telescope in a subfolder of `~/org`.
-- @param base_folder: string: base folder
M.open_org_file_telescope = function(base_folder)
	local action_state = require("telescope.actions.state")
	local actions = require("telescope.actions")
	local Path = require("plenary.path")

	local folder = vim.fn.expand("~/org/") .. base_folder .. "/"

	-- Archive can be gross, need to explicitly ask for it, otherwise we'll hide it.
	local ignore_files = {}
	if base_folder ~= "archive" then
		ignore_files = { "~/archive" }
	end

	require("telescope.builtin").find_files({
		file_ignore_patterns = ignore_files,
		attach_mappings = function(prompt_bufnr, map)
			-- Creates a file using the telescope input prompt.
			-- Useful to quickly create a file if nothing exists.
			local create_file = function()
				-- It ain't pretty... But maybe it's good enough...? T.T
				local current_picker = action_state.get_current_picker(prompt_bufnr)
				local input = folder .. current_picker:_get_prompt() .. ".dj"

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

return M
