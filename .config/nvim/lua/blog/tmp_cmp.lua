print("LOADED")

local source = {}

-- function source:is_available()
-- 	return true
-- end

function source:complete(params, callback)
	local cursor_before_line = params.context.cursor_before_line

	-- Regex matches `](/`.
	-- The `[^%)]*$` ensures that we continue completion even
	-- after we've typed some characters.
	if string.match(cursor_before_line, "%]%(/[^%)]*$") then
		local items = {
			{
				label = "Rewriting my Neovim config in Lua",
				insertText = "/blog/2023/10/01/rewriting_my_neovim_config_in_lua/",
				-- Text to filter for, works like `ordinal` for telescope.
				filterText = "/blog/2023/10/01/rewriting_my_neovim_config_in_lua/|Rewriting my Neovim config in Lua",
			},
		}
		callback(items)
	else
		-- `callback` should always be called.
		callback({})
	end
end

function source:get_trigger_characters()
	return { "/" }
end

require("cmp").register_source("blog", source)
