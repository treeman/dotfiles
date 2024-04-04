local pickers = require("telescope.pickers")
local finders = require("telescope.finders")
local config = require("telescope.config").values

local M = {}

M.show_blog_posts = function(opts)
	print("show blog posts")
	local res = require("blog/server").list_posts()

	pickers.new(opts, {
		finder = finders.new_table({
			"Yes",
			"No",
		}),
		sorter = config.generic_sorter(opts),
	}):find()
end

M.show_blog_posts()

-- Browse posts
-- Browse drafts
-- Browse tags

return M
