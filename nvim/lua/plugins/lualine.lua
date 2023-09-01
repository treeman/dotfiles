--- @param trunc_width number trunctates component when screen width is less then trunc_width
--- @param trunc_len number truncates component to trunc_len number of chars
--- @param hide_width number hides component when window width is smaller then hide_width
--- @param no_ellipsis boolean whether to disable adding '...' at end after truncation
--- return function that can format the component accordingly
local function trunc(trunc_width, trunc_len, hide_width, no_ellipsis)
	return function(str)
		local win_width = vim.fn.winwidth(0)
		if hide_width and win_width < hide_width then
			return ""
		elseif trunc_width and trunc_len and win_width < trunc_width and #str > trunc_len then
			return str:sub(1, trunc_len) .. (no_ellipsis and "" or "...")
		end
		return str
	end
end

local function lsp_info()
	-- TODO this can sometimes be very wordy
	if vim.lsp.get_active_clients() == nil then
		return ""
	else
		return require("lsp-status").status()
	end
end

local function spell()
	if vim.opt.spell:get() then
		return table.concat(vim.opt.spelllang:get(), ",")
	else
		return ""
	end
end

-- Note that "0x%0B" is bugged, but works with the %b prefix
local charhex = "%b 0x%B"
-- Show location s column:row/total_rows
local location = "%c:%l/%L"

-- Theme is hard.
-- I dumped the "auto" theme and tried to adjust some things that bothered me a little.
local colors = require("melange.palettes.dark")
local custom_theme = {
	normal = {
		a = { bg = "#463f3b", fg = "#96918e", gui = "bold" },
		b = { bg = "#2d2825", fg = "#7d7672" },
		c = { bg = "#393430", fg = colors.a.com },
	},
	inactive = {
		a = { bg = "#463f3b", fg = "#96918e", gui = "bold" },
		b = { bg = "#2d2825", fg = "#7d7672" },
		c = { bg = "#393430", fg = colors.a.ui },
	},
	command = {
		a = { bg = colors.c.green, fg = "#2d2825", gui = "bold" },
		b = { bg = "#2d2825", fg = colors.c.green },
		c = { bg = "#393430", fg = colors.a.com },
	},
	terminal = {
		a = { bg = colors.c.cyan, fg = "#2d2825", gui = "bold" },
		b = { bg = "#2d2825", fg = colors.c.cyan },
		c = { bg = "#393430", fg = colors.a.com },
	},
	insert = {
		a = { bg = colors.c.cyan, fg = "#2d2825", gui = "bold" },
		b = { bg = "#2d2825", fg = colors.c.cyan },
		c = { bg = "#393430", fg = colors.a.com },
	},
	replace = {
		a = { bg = colors.c.magenta, fg = "#2d2825", gui = "bold" },
		b = { bg = "#2d2825", fg = colors.c.magenta },
		c = { bg = "#393430", fg = colors.a.com },
	},
	visual = {
		a = { bg = colors.c.yellow, fg = "#2d2825", gui = "bold" },
		b = { bg = "#2d2825", fg = colors.c.yellow },
		c = { bg = "#393430", fg = colors.a.com },
	},
}

local filename = {
	"filename",
	path = 1,
}

local function config()
	local lualine = require("lualine")
	local lsp_status = require("lsp-status")

	lsp_status.config({
		current_function = false,
		show_filename = false,
		diagnostics = false,
	})
	lsp_status.register_progress()

	lualine.setup({
		options = {
			theme = custom_theme,
		},
		sections = {
			lualine_c = { filename },
			lualine_x = {
				{
					lsp_info,
					fmt = trunc(200, 80, 80, false),
				},
				spell,
				"encoding",
				"filetype",
			},
			lualine_y = { charhex },
			lualine_z = { location },
		},
		inactive_sections = {
			lualine_a = {},
			lualine_b = {},
			lualine_c = { filename },
			lualine_x = {},
			lualine_y = {},
			lualine_z = { location },
		},
	})
end

return {
	"nvim-lualine/lualine.nvim",
	dependencies = {
		"nvim-tree/nvim-web-devicons",
		"nvim-lua/lsp-status.nvim",
	},
	config = config,
	lazy = false,
}
