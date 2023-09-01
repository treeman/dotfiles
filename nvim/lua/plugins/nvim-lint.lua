local function config()
	require("lint").linters_by_ft = {
		markdown = { "vale" },
	}

	vim.api.nvim_create_autocmd({ "BufWritePost", "BufReadPost" }, {
		callback = function()
			require("lint").try_lint()
		end,
	})
end

return {
	"mfussenegger/nvim-lint",
	config = config,
	event = "BufReadPre",
}
