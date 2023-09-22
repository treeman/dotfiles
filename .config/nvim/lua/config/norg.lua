local M = {}

-- Find files using telescope in a subfolder of `~/norg`.
-- @param base_folder: string: base folder
M.open_norg = function(base_folder)
	local action_state = require("telescope.actions.state")
	local actions = require("telescope.actions")
	local Path = require("plenary.path")

	local folder = vim.fn.expand("~/norg/") .. base_folder .. "/"

	require("telescope.builtin").find_files({
		attach_mappings = function(prompt_bufnr, map)
			-- Creates a file using the telescope input prompt.
			-- Useful to quickly create a file if nothing exists.
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

-- Open a weekly journal in `~/norg/areas/weekly_journal/`.
-- Create using a template from `~/norg/areas/weekly_journal/template.norg` unless it exists.
M.open_weekly_journal = function()
	local Path = require("plenary.path")

	-- Should probably make this more general in the future.
	local pwd = vim.fn.expand("~/norg/areas/weekly_journal/")
	local journal_file = pwd .. os.date("w%W") .. ".norg"

	local file = Path:new(journal_file)
	if not file:exists() then
		local template = Path:new(pwd .. "template.norg")
		template:copy({ destination = file, override = false })
	end

	vim.cmd("e " .. journal_file .. "| w")
end

return M
