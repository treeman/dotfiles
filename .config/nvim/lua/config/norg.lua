local M = {}

M.open_norg = function(base_folder)
	local action_state = require("telescope.actions.state")
	local actions = require("telescope.actions")
	local Path = require("plenary.path")

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

-- Should probably make this more general in the future.
M.open_weekly_journal = function()
	local Path = require("plenary.path")
	local pwd = vim.fn.expand("~/norg/areas/weekly_journal/")
	local journal_file = pwd .. os.date("w%W") .. ".norg"

	local file = Path:new(journal_file)
	if not file:exists() then
		local res, err = vim.loop.fs_copyfile(pwd .. "template.norg", journal_file)
		if not res then
			print("error copying template: " .. err)
			return
		end
	end

	vim.cmd("e " .. journal_file .. "| w")
end

return M
