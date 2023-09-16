return {
	"goolord/alpha-nvim",
	lazy = false,
	-- dependencies = { "nvim-tree/nvim-web-devicons" },
	opts = require("config.dashboard"),
	config = function(_, dashboard)
		dashboard.section.fortune.val = require("alpha.fortune")(60)

		require("alpha").setup(dashboard.config)
		vim.api.nvim_create_autocmd("User", {
			callback = function()
				local stats = require("lazy").stats()
				local ms = math.floor(stats.startuptime * 100) / 100
				dashboard.section.lazy.val = "Lazy-loaded "
					.. stats.loaded
					.. " of "
					.. stats.count
					.. " plugins in "
					.. ms
					.. "ms"
				pcall(vim.cmd.AlphaRedraw)
			end,
		})
	end,
}
