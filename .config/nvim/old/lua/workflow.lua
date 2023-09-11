local function file_exists(filepath)
	local f = io.open(filepath, "r")

	if f ~= nil then
		f:close()
		return true
	else
		return false
	end
end

-- Should probably make this more general in the future.
function _G.weekly_journal()
	local pwd = vim.fn.expand("~/norg/areas/weekly_journal/")
	local journal_file = pwd .. os.date("w%W") .. ".norg"

	if not file_exists(journal_file) then
		local res, err = vim.loop.fs_copyfile(pwd .. "template.norg", journal_file)
		if not res then
			print("error copying template: " .. err)
			return
		end
	end

	vim.cmd("e " .. journal_file .. "| w")
end
